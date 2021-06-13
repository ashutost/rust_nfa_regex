//! # NFA Regex
//!
//! A non finite automata regular expression engine library.
//! NFA Regex concept: https://swtch.com/~rsc/regexp/regexp1.html
//! 
//! Implementation of NFA regex engine:
//! 1.   Convert infix string regex pattern to postfix pattern also called reverse polish notation.
//!      https://en.wikipedia.org/wiki/Reverse_Polish_notation
//!      https://www.free-online-calculator-use.com/infix-to-postfix-converter.html
//! 2.   Build the NFA regex engine from the postfix notation.
//!      
//! List of supported operators for NFA regex:
//! | Operator     | Description
//! |--------------|---------------------------------------------------------------------------------------------------------------------------------------------------
//! | \            | Escape character
//! | -            | Range operator. E.g. [0-9a-z]
//! | .            | Any displayable character (also called metacharacter)
//! | []           | Square bracket, Ors characters inside
//! | ()           | Round bracket, groups characters (concats)
//! | *            | Zero or more character repeatition
//! | +            | One or more charatcer repeatition
//! | ?            | Zero or one character repeatition
//! | {m,n} or {n} | Curly bracket for character repeatition with min and max limit count
//! | ^            | Exclude characters used only inside [] at the begin. E.g. [^ab] - string must not have a and b.
//! | |            | ORs characters or group. (Do not use inside [])
//! |--------------|---------------------------------------------------------------------------------------------------------------------------------------------------
//!
//! Operator precedance and associativity, table taken from unix: (Not all operators are supported in below implementation.)
//! +---+----------------------------------------------------------+
//! |   |             ERE Precedence (from high to low)            |
//! +---+----------------------------------------------------------+
//! | 1 | Collation-related bracket symbols | [==] [::] [..]       |
//! | 2 | Escaped characters                | \<special character> |
//! | 3 | Bracket expression                | []                   |
//! | 4 | Grouping                          | ()                   |
//! | 5 | Single-character-ERE duplication  | * + ? {m,n}          |
//! | 6 | Concatenation                     |                      |
//! | 7 | Anchoring                         | ^ $                  |
//! | 8 | Alternation                       | |                    |
//! +---+-----------------------------------+----------------------+

/// Regex operator enum. Almost all operators are single character except {m,n} operator, hence the need of
/// enum to store these operators as enum state. So, repeat operators are stored separetely and other single
/// character operators stores as single character wrapped in enum variant.
#[derive(Debug, Clone, Eq, PartialEq)]
enum RegexOp {
    // Repeat operators with following cases:
    // + => (1, MAX)
    // * => (0, MAX)
    // ? => (0, 1)
    // {m,n} => (m, n) where m < n, m >= 0, n > 0
    // {n} => (n, n) where n > 1
    ByRp(isize, isize),
    // All other single character operators
    Op(char),
}

/// Out index pointer wrapper used in Empty/Split NFA state.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum StateOut {
    Out(usize),                        // Used when no limit on repeat count i.e. indefinite repeat. Only stores index into NFA engine vector.
    Repeat(usize, isize, isize, isize) // Used when there is limit on repeat count. Stores index into NFA engine vector, min and max repeat values and current repeat count
}

/// NFA regex engine states
#[derive(Debug, Clone, Eq, PartialEq)]
enum State {
    AnyChar(usize),            // '.' metacharacter to include all displayable characters with out index pointer.
    Char(char, usize),         // Input character must match this character, out index points to next state.
    NotChar(char, usize),      // Input character must not match this character, out index points to next state.
    Range(char, char, usize),  // Input character must fall within range of characters, out index points to next state.
    Oprtr(RegexOp),            // Determines the function or type of operation to be performed over last fragment of NFA regex state/states. This is temporary state.
    EmptySplit(Vec<StateOut>), // Intermediate state with no value. Becomes split when building nfa engine state for '|' otherwise becomes empty.
    Fragment(usize, usize),    // Temporary state called NFA fragment to store start and end of partially built NFA engine while NFA engine is being parsed and built.
    Start(usize),              // Determines start of NFA engine, out points to first state.
    Match                      // Determines match or end of NFA engine.
}

// Type alias used locally for easy code readability
type Postfix = Vec<State>;
type NfaEngine = Vec<State>;

/// NFA regex engine
#[derive(Debug, Clone)]
pub struct Regex {
    nfa_engine: NfaEngine, // List of NFA states in vector connected together to make NFA regex engine
    start: usize,          // Index into nfa_engine at which the engine starts.
}

/// Implementing regex methods or functions
impl Regex {
    /// Takes infix regex pattern string and turns it to postfix pattern. Also throws syntax errors.
    fn infix_to_postfix(s: &str) -> Result<Postfix, &'static str> {
        // This is compile stage where we generate syntax errors i.e. check the grammar and convert expression to postfix for easy linking and generating regex engine.
        if s.is_empty() { return Err("Syntax Error: Input pattern string is empty."); }

        let mut postfix: Postfix = vec![]; // Stores pattern in postfix format than in infix format.
        let mut stack: Vec<RegexOp> = vec![]; // Temporarily stores all operators as per their precedance and associativity. Almost all ops are left to right associative.
        let mut square_brkt = false; // True if '[' encountered, false when ']' encountered after '['.
        let mut char_pushed = false; // True when last thing pushed to postfix is char or closing bracket ')' or ']', false otherwise.
        let mut not_char = false; // True when '^' is encountered as it applies to entire group, so it has to stay true till end of group.
        let mut round_brkt = 0; // Counts that number of opening brackets equal to closing one. Does not verify that brackets are at right place and no syntax error.
        let mut chars = s.chars().fuse(); // Create char iterator which stops as soon as first None is returned

        // Pushes operator to stack respecting operator precedance and associativity.
        let push_op_to_stack = |op: RegexOp, st: &mut Vec<RegexOp>, p: &mut Postfix| {
            use RegexOp::*;
            match op {
                ByRp(_, _) => {
                    // If its repeat operator like *, +, ?, {m,n}, then pop all earlier repeat ops from stack and push to postfix and finally push our new repeat op.
                    // All ops are left to right associative
                    while let Some(in_op) = st.pop() {
                        match in_op {
                            ByRp(_, _) => p.push(State::Oprtr(in_op)), // Pop all same and high precedance ops and push to postfix
                            Op(_) => { st.push(in_op); break; } // Do nothing as other ops are low precedance and high can push over low precedance.
                        }
                    }
                }
                Op(c) => {
                    match c {
                        '#' => {
                            // Repeat/bypass op precedes # and is left to right associative. Pop high and same precedance ops and push into postfix. Finally push '#'
                            while let Some(in_op) = st.pop() {
                                match in_op {
                                    ByRp(_, _) | Op('#') => p.push(State::Oprtr(in_op)),
                                    _ => { st.push(in_op); break; }
                                }
                            }
                        }
                        '|' => {
                            // # precedes |, same behaviour for | as for #
                            while let Some(in_op) = st.pop() {
                                match in_op {
                                    ByRp(_, _) | Op('#') | Op('|') => p.push(State::Oprtr(in_op)),
                                    _ => { st.push(in_op); break; }
                                }
                            }
                        }
                        _ => () // Nothing to be done for other operators.
                    }
                }
            }
            st.push(op);
        };

        while let Some(c) = chars.next() {
            // Operators matched in precedance for readability.
            match c {
                '-' => {
                    // The operator must be inside [] and must have LHS char pushed, otherwise throw error.
                    if square_brkt && char_pushed {
                        push_op_to_stack(RegexOp::Op(c), &mut stack, &mut postfix);
                    } else {
                        return Err("Syntax Error: Either missing LHS char in range or used '-' operator outside [].");
                    }
                    // Pushing an operator must make this flag false
                    char_pushed = false;
                }
                '[' | '(' => {
                    // Cannot have another [ or () operator inside existing [ operator.
                    if square_brkt {
                        match c {
                            '[' => return Err("Syntax Error: Nested square brackets '[]' not allowed."),
                            '(' => return Err("Syntax Error: Grouping '()' inside square brackets '[]' are not allowed."),
                            _ => ()
                        }
                    } else {
                        match c {
                            '[' => square_brkt = true,
                            '(' => round_brkt += 1,
                            _ => ()
                        }
                    }

                    // Pop all operators before pushing '[' or '('.
                    while let Some(op) = stack.pop() {
                        // Every unary operator (E.g. *, +, ?, {m,n}) and ], ) ops along with character should be followed by # when new [ or ( starts.
                        // E.g. [\\+\\-]?[0-9]+ => After unary ? operator # must be pushed to concat [\\+\\-]? to [0-9]+.
                        // Hence, making char_pushed true in below operation of operator poping out of stack and pushed to postfix.
                        use RegexOp::*;
                        match op {
                            ByRp(_, _) => { postfix.push(State::Oprtr(op)); char_pushed = true; }
                            _ => { stack.push(op); break; }
                        }
                    }
                    if char_pushed {
                        push_op_to_stack(RegexOp::Op('#'), &mut stack, &mut postfix);
                    }
                    stack.push(RegexOp::Op(c));
                    char_pushed = false;
                }
                ']' => {
                    // Close square bracket if opened, otherwise error out. Also, make sure only operators allowed inside [] are '-' and '|'.
                    if !square_brkt {
                        return Err("Syntax Error: No matching '[' found.");
                    }
                    use RegexOp::*;
                    while let Some(op) = stack.pop() {
                        match op {
                            Op('-') | Op('|') => postfix.push(State::Oprtr(op)),
                            Op('[') => { square_brkt = false; break; }
                            _ => return Err("Syntax Error: Invalid operator inside [].")
                        }
                    }
                    char_pushed = true;
                    not_char = false;
                }
                ')' => {
                    // Make sure ) is after ( and not inside []. Pop all operators till ( is reached and push them to postfix.
                    if square_brkt {
                        return Err("Syntax Error: '()' not allowed inside '[]'.");
                    }
                    use RegexOp::*;
                    while let Some(op) = stack.pop() {
                        match op {
                            Op('(') => { round_brkt -= 1; break; }
                            _ => postfix.push(State::Oprtr(op))
                        }
                    }
                    char_pushed = true;
                }
                '*' | '+' | '?' => {
                    // Unary operator with unknown repetition bound
                    // When m = 0, n = isize::MAX, then bypass is allowed with theoretical infinite repeatition
                    // When m = 1, n = isize::MAX, then bypass is not allowed with theoretical infinite repeatition
                    // When m = 0, n = 1, then bypass is allowed with no repeatition.
                    // When m = -1, n = -1, then its error. This never happens as the match arm we are in already took care of that but the following below
                    //   match arm fails to compile if _ is not used.
                    let op = match c {
                        '*' => RegexOp::ByRp(0, isize::MAX),
                        '+' => RegexOp::ByRp(1, isize::MAX),
                        '?' => RegexOp::ByRp(0, 1),
                        _ => RegexOp::ByRp(-1, -1)
                    };
                    push_op_to_stack(op, &mut stack, &mut postfix);
                    char_pushed = false;
                }
                '{' => {
                    // Unary operator with fixed number of repeatition.
                    // When m = 0 and n > 1 and n < isize::MAX, then bypass is allowed with n repeatitions.
                    // When m = 1 and n > 1 and n < isize::MAX, then bypass is not allowed with n repeatitions.
                    // We read till } operator and then parse numbers to create Repeat operator.
                    let mut nums = String::new();
                    while let Some(ch) = chars.next() {
                        if ch == '}' {
                            break;
                        } else {
                            nums.push(ch);
                        }
                    }
                    let nums: Vec<&str> = nums.split(',').collect();
                    let m: isize;
                    let n: isize;
                    // When only 1 number is given then m = n = num in {num}
                    // When 2 numbersare given, then m = num1, n = num2 in {num1,num2}
                    // Any different number count, its a syntax error.
                    match nums.len() {
                        1 => {
                            if nums[0].is_empty() {
                                return Err("Syntax Error: The value bound m and/or n cannot be empty. Usage {m,n} or {n}, where m > 0 and n > 0.");
                            }
                            m = nums[0].parse::<isize>().unwrap();
                            n = m;
                            if n <= 1 {
                                return Err("Syntax Error: Apply {n} repeat, iff n > 1.");
                            }
                        }
                        2 => {
                            if nums[0].is_empty() || nums[1].is_empty() {
                                return Err("Syntax Error: The value bound m and/or n cannot be empty. Usage {m,n} or {n}, where m > 0 and n > 0.");
                            }
                            m = nums[0].parse::<isize>().unwrap();
                            n = nums[1].parse::<isize>().unwrap();
                            if m > n || m < 0 || n < 1 {
                                return Err("Syntax Error: Apply {m,n} repeat, iff m < n, m >= 0, n > 0");
                            }
                        }
                        _ => return Err("Syntax Error: Use either\n{m,n}, iff m < n, m >= 0, n > 0\nor\n{n}, iff n > 1.")
                    }
                    push_op_to_stack(RegexOp::ByRp(m, n), &mut stack, &mut postfix);
                    char_pushed = false;
                }
                '^' => {
                    // Used only as not operator on characters to disallow them in regex check and is allowed only inside [].
                    if square_brkt {
                        not_char = true;
                    } else {
                        return Err("Syntax Error: '^' is only allowed inside '[]'.");
                    }
                    char_pushed = false;
                }
                '|' => {
                    push_op_to_stack(RegexOp::Op(c), &mut stack, &mut postfix);
                    char_pushed = false;
                }
                _ => {
                    // Usual character, not an operator. Also includes operators deals with characters directly.
                    if char_pushed {
                        // Always push concat or '#' op except if the character is inside [], in that case
                        // if stack has last op as '-' then push that to postfix otherwise push '|' operator
                        if square_brkt {
                            if *stack.last().unwrap() == RegexOp::Op('-') {
                                postfix.push(State::Oprtr(stack.pop().unwrap()));
                            }
                            push_op_to_stack(RegexOp::Op('|'), &mut stack, &mut postfix);
                        } else {
                            push_op_to_stack(RegexOp::Op('#'), &mut stack, &mut postfix);
                        }
                    }

                    let mut ch: char = c;
                    let mut anychar = false;
                    // Anything after escape character is character, irrespective of it being followed by operator or character.
                    match c {
                        '\\' => ch = chars.next().unwrap(),
                        '.' => {
                            if not_char {
                                return Err("Syntax Error: '^' cannot be applied to '.' metacharacter.");
                            }
                            anychar = true
                        }
                        _ => ch = c
                    }
                    // Just in case, pop out any unary operator and push it to postfix. As unary operator applies to characters before this new character we are pushing to postfix.
                    let mut add_concat = false;
                    while let Some(op) = stack.pop() {
                        use RegexOp::*;
                        match op {
                            ByRp(_, _) => { postfix.push(State::Oprtr(op)); add_concat = true; },
                            _ => { stack.push(op); break; }
                        }
                    }
                    // In case we have poped out unary operator, we must add concat operator in stack before pushing character to postfix.
                    if add_concat {
                        push_op_to_stack(RegexOp::Op('#'), &mut stack, &mut postfix);
                    }
                    // Finally, now push char to postfix.
                    if anychar {
                        postfix.push(State::AnyChar(usize::MAX));
                    } else if not_char {
                        postfix.push(State::NotChar(ch, usize::MAX));
                    } else {
                        postfix.push(State::Char(ch, usize::MAX));
                    }
                    char_pushed = true;
                }
            }
        }
        
        // Make sure no square bracket or round bracket exists after above loop
        if square_brkt {
            return Err("Syntax Error: Missing ']' bracket.");
        }
        if round_brkt > 0 || round_brkt < 0 {
            return Err("Syntax Error: Missing '(' or ')' grouping operators.");
        }

        // If stack has any operators, append them to postfix.
        while let Some(op) = stack.pop() {
            postfix.push(State::Oprtr(op));
        }

        Ok(postfix)
    }

    /// Pretty printing for postfix pattern expression only used for debugging purpose
    fn _print_postfix(infix: &str, postfix: &Postfix) -> String {
        use State::*;
        let mut postfix_str = format!("\nPrinting postfix: Abbreviation: CH - Character, TY - Type,\nTypes are: o - operator, c - char, n - not char\nInfix Pattern: {}\nCH: ", infix);
        for s in postfix.iter() {
            match s {
                AnyChar(_) => postfix_str.push_str(". "),
                Char(c, _) | NotChar(c, _) => postfix_str.push_str(&format!("{} ", c)),
                Oprtr(op) => match op {
                    RegexOp::ByRp(m, n) => {
                        if *m == 0 && *n == 1 {
                            postfix_str.push_str("? ");
                        } else if *m == 0 && *n == isize::MAX {
                            postfix_str.push_str("* ");
                        } else if *m == 1 && *n == isize::MAX {
                            postfix_str.push_str("+ ");
                        } else {
                            // postfix_str.push_str(&format!("\{{},{}\}", *m, *n));
                            postfix_str.push_str("@ "); // Replacing multiple chars with single char
                        }
                    }
                    RegexOp::Op(c) => postfix_str.push_str(&format!("{} ", c))
                }
                _ => ()
            }
        }

        postfix_str.push_str("\nTY: ");
        for s in postfix.iter() {
            match s {
                AnyChar(_) | Char(_, _) => postfix_str.push_str("c "),
                NotChar(_, _) => postfix_str.push_str("n "),
                Oprtr(_) => postfix_str.push_str("o "),
                _ => ()
            }
        }

        postfix_str.push('\n');
        postfix_str
    }

    /// Takes postfix pattern expression and builds NFA regex engine from it. Returns parsing error if fails.
    fn postfix_to_nfa(postfix: Postfix) -> Result<Self, &'static str> {
        use State::*;
        use RegexOp::*;
        use StateOut::*;

        // This is where we parse postfix expression and generate regex engine, just like linking and generating binary.
        if postfix.is_empty() { return Err("Parse Error: Empty postfix expression."); }

        let mut nfa_engine: NfaEngine = vec![]; // NFA regex engine vector comprised of NFA states
        let mut stack: NfaEngine = vec![]; // Temporary stack of NFA states to help build NFA engine
        let mut s1: State; // Temporary state s1 and s2
        let mut s2: State;
        let mut i0: usize = 0; // Temporary index i0 and i1
        let mut i1: usize = 0;

        // When deleting Empty/Split state to reduce unnecesary duplicate states since they are merged then
        // - update index from old to new if its old index as old data merged to new index
        //   OR
        // - decrement index by 1 if it is greater than old index
        let dec_1_if_ge = |nfa: &mut NfaEngine, old_idx: usize, new_idx: usize| {
            nfa.iter_mut().for_each(|s| {
                match s {
                    AnyChar(o) | Char(_, o) | NotChar(_, o) | Range(_, _, o) =>
                    if *o > old_idx { *o -= 1 } else if *o == old_idx { *o = new_idx }
                    EmptySplit(v) => v.into_iter().for_each(|o| { match o {
                        Out(ao) | Repeat(ao, _, _, _) => if *ao > old_idx { *ao -= 1 } else if *ao == old_idx { *ao = new_idx } }}),
                    _ => ()
                }
            });
        };

        // True if vector inside Empty/Split state has given index `i` irrespective of wrapped enum variant, false otherwise
        let vec_has_i = |v: &Vec<StateOut>, i: usize| {
            v.iter().any(|e| { match e { Out(o) | Repeat(o, _, _, _) => *o == i } })
        };

        for s in postfix.into_iter() {
            match s {
                AnyChar(_) | Char(_, _) | NotChar(_, _) => stack.push(s), // Push character types as is to stack for later processing
                Oprtr(op) => match op {
                    ByRp(m, n) => {
                        s1 = stack.pop().unwrap();
                        match s1 {
                            AnyChar(ref mut o) | Char(_, ref mut o) | NotChar(_, ref mut o) | Range(_, _, ref mut o) => {
                                // Push empty/split -> chara type state -> empty/split
                                i0 = nfa_engine.len();
                                // If min repeat value is 0, then connect bypass otherwise no bypass.
                                if m == 0 {
                                    nfa_engine.push(EmptySplit(vec![Out(i0 + 1), Out(i0 + 2)]));
                                } else {
                                    nfa_engine.push(EmptySplit(vec![Out(i0 + 1)]));
                                }
                                *o = i0 + 2;
                                nfa_engine.push(s1);
                                // - If max repeat value is MAX isize value then do indefinite repeat using Out.
                                // - If max repeat value is less than MAX but greater than 1 then do definite repeat using Repeat.
                                // - Otherwise no repeat, just push Empty/Split with empty vector to complete the repeat operator implementation.
                                if n == isize::MAX {
                                    nfa_engine.push(EmptySplit(vec![Out(i0)]));
                                } else if n > 1 {
                                    nfa_engine.push(EmptySplit(vec![Repeat(i0, m, n, 0)]));
                                } else {
                                    nfa_engine.push(EmptySplit(vec![]));
                                }
                                i1 = nfa_engine.len() - 1;
                            }
                            Fragment(b, e) => match (&nfa_engine[b], &nfa_engine[e]) {
                                (EmptySplit(bv), EmptySplit(ev)) => {
                                    // Multiple repeat operators to fragment not allowed
                                    if vec_has_i(bv, e) || vec_has_i(ev, b) {
                                        return Err("Parse Error: Cannot apply repeat or duplication operator twice.");
                                    }
                                    // If end Empty/Split state is not empty i.e. points back to state after begin state, then push new Empty/Split state at the end.
                                    // Otherwise, use existing Empty/Split states at begin and end indices.
                                    i0 = b;
                                    if !ev.is_empty() {
                                        i1 = nfa_engine.len();
                                        if let EmptySplit(ve) = &mut nfa_engine[e] {
                                            ve.push(Out(i1));
                                        }
                                        if n == isize::MAX {
                                            nfa_engine.push(EmptySplit(vec![Out(b)]));
                                        } else if n > 1 {
                                            nfa_engine.push(EmptySplit(vec![Repeat(b, m, n, 0)]));
                                        } else {
                                            nfa_engine.push(EmptySplit(vec![]));
                                        }
                                    } else {
                                        if let EmptySplit(ve) = &mut nfa_engine[e] {
                                            // Empty/Split already exists at the end, nothing to be done in case n == 1 i.e. no repeat.
                                            if n == isize::MAX { ve.push(Out(b)); } else if n > 1 { ve.push(Repeat(b, m, n, 0)); }
                                        }
                                        i1 = e;
                                    }
                                    // Below code is common for both if and else part i.e. bypass from begin is common operation
                                    if let EmptySplit(vb) = &mut nfa_engine[b] {
                                        if m == 0 { vb.push(Out(i1)); }
                                    }
                                }
                                (_, _) => {
                                    // Irrespective of begin and end index states, push 2 Empty/Split states at begin and end as
                                    // Empty/Split -> {Fragment (Need no push as it already exists in nfa engine)} -> Empty/Split
                                    i0 = nfa_engine.len();
                                    i1 = i0 + 1;
                                    match &mut nfa_engine[e] {
                                        AnyChar(o) | Char(_, o) | NotChar(_, o) | Range(_, _, o) => *o = i1,
                                        EmptySplit(v) => v.push(Out(i1)),
                                        _ => ()
                                    }
                                    if m == 0 {
                                        nfa_engine.push(EmptySplit(vec![Out(b), Out(i1)]));
                                    } else {
                                        nfa_engine.push(EmptySplit(vec![Out(b)]));
                                    }
                                    if n == isize::MAX {
                                        nfa_engine.push(EmptySplit(vec![Out(i0)]));
                                    } else if n > 1 {
                                        nfa_engine.push(EmptySplit(vec![Repeat(i0, m, n, 0)]));
                                    } else {
                                        nfa_engine.push(EmptySplit(vec![]));
                                    }
                                }
                            }
                            _ => ()
                        }
                        stack.push(Fragment(i0, i1));
                    }
                    Op('-') => {
                        // First pop from stack is RHS and second pop is LHS for any operator.
                        s2 = stack.pop().unwrap(); // Pop out RHS
                        s1 = stack.pop().unwrap(); // Pop out LHS
                        match (s1, s2) {
                            (Char(b, _), Char(e, _)) => {
                                if b >= e {
                                    return Err("Parse Error: Characters not in range in [b-e].");
                                }
                                stack.push(Range(b, e, usize::MAX));
                            }
                            (_, _) => return Err("Parse Error: Invalid state type for '-' operator.")
                        }
                    }
                    Op('#') => {
                        s2 = stack.pop().unwrap();
                        s1 = stack.pop().unwrap();
                        match (&mut s1, &mut s2) {
                            // Why is below not supported? E0658
                            // or-patterns syntax is experimental
                            // see issue #54883 <https://github.com/rust-lang/rust/issues/54883> for more information rustc(E0658)
                            // (AnyChar(ref mut o1) | Char(_, ref mut o1) | NotChar(_, ref mut o1) | Range(_, _, ref mut o1),
                            // AnyChar(ref mut o2) | Char(_, ref mut o2) | NotChar(_, ref mut o2) | Range(_, _, ref mut o2)) => {}
                            (AnyChar(o), AnyChar(_)) |
                            (AnyChar(o), Char(_, _)) |
                            (AnyChar(o), NotChar(_, _)) |
                            (AnyChar(o), Range(_, _, _)) |
                            (Char(_, o), AnyChar(_)) |
                            (Char(_, o), Char(_, _)) |
                            (Char(_, o), NotChar(_, _)) |
                            (Char(_, o), Range(_, _, _)) |
                            (NotChar(_, o), AnyChar(_)) |
                            (NotChar(_, o), Char(_, _)) |
                            (NotChar(_, o), NotChar(_, _)) |
                            (NotChar(_, o), Range(_, _, _)) |
                            (Range(_, _, o), AnyChar(_)) |
                            (Range(_, _, o), Char(_, _)) |
                            (Range(_, _, o), NotChar(_, _)) |
                            (Range(_, _, o), Range(_, _, _)) => {
                                i0 = nfa_engine.len();
                                i1 = nfa_engine.len() + 1;
                                *o = i1;
                                nfa_engine.push(s1);
                                nfa_engine.push(s2);
                            }
                            (Fragment(b, e), AnyChar(_)) |
                            (Fragment(b, e), Char(_, _)) |
                            (Fragment(b, e), NotChar(_, _)) |
                            (Fragment(b, e), Range(_, _, _)) => {
                                i0 = *b;
                                i1 = nfa_engine.len();
                                nfa_engine.push(s2);
                                match &mut nfa_engine[*e] {
                                    AnyChar(o) | Char(_, o) | NotChar(_, o) | Range(_, _, o) => *o = i1,
                                    EmptySplit(v) => v.push(Out(i1)),
                                    _ => ()
                                }
                            }
                            (AnyChar(o), Fragment(b, e)) |
                            (Char(_, o), Fragment(b, e)) |
                            (NotChar(_, o), Fragment(b, e)) |
                            (Range(_, _, o), Fragment(b, e)) => {
                                *o = *b;
                                i0 = nfa_engine.len();
                                nfa_engine.push(s1);
                                i1 = *e;
                            }
                            (Fragment(bl, el), Fragment(br, er)) => {
                                // l at end of index means LHS and r means RHS.
                                match &nfa_engine[*br] {
                                    AnyChar(_) | Char(_, _) | NotChar(_, _) | Range(_, _, _) => match &mut nfa_engine[*el] {
                                        AnyChar(o) | Char(_, o) | NotChar(_, o) | Range(_, _, o) => *o = *br,
                                        EmptySplit(v) => v.push(Out(*br)),
                                        _ => ()
                                    }
                                    EmptySplit(tv) => {
                                        let ttv = tv.clone();
                                        match &mut nfa_engine[*el] {
                                            AnyChar(o) | Char(_, o) | NotChar(_, o) | Range(_, _, o) => *o = *br,
                                            EmptySplit(v) => {
                                                // If LHS fragment end = Empty/Split and RHS fragment begin = Empty/Split then
                                                // Push all data from RHS fragment begin vector to LHS fragment end vector of Empty/Split state.
                                                // Delete RHS fragment begin state. This is to avoid multiple Empty/Split state when one is enough to do the job.
                                                // Append RHS fragment begin Empty/Split vector values to LHS fragment end Empty/Split vector with no element duplication
                                                ttv.into_iter().for_each(|te| { if !v.contains(&te) { v.push(te); }});
                                                // Update out index of states inside nfa_engine vector.
                                                dec_1_if_ge(&mut nfa_engine, *br, *el);
                                                // If index is greater than or equal to br, decrement by 1
                                                *bl = if *bl > *br { *bl - 1 } else if *bl == *br { *el } else { *bl };
                                                *er = if *er > *br { *er - 1 } else if *bl == *br { *el } else { *er };
                                                // Remove the empty/split state at RHS fragment end index
                                                nfa_engine.remove(*br);
                                            }
                                            _ => ()
                                        }
                                    }
                                    _ => ()
                                }
                                i0 = *bl;
                                i1 = *er;
                            }
                            _ => ()
                        }
                        stack.push(Fragment(i0, i1));
                    }
                    Op('|') => {
                        s2 = stack.pop().unwrap();
                        s1 = stack.pop().unwrap();
                        match (&mut s1, &mut s2) {
                            (AnyChar(ol), AnyChar(or)) |
                            (AnyChar(ol), Char(_, or)) |
                            (AnyChar(ol), NotChar(_, or)) |
                            (AnyChar(ol), Range(_, _, or)) |
                            (Char(_, ol), AnyChar(or)) |
                            (Char(_, ol), Char(_, or)) |
                            (Char(_, ol), NotChar(_, or)) |
                            (Char(_, ol), Range(_, _, or)) |
                            (NotChar(_, ol), AnyChar(or)) |
                            (NotChar(_, ol), Char(_, or)) |
                            (NotChar(_, ol), NotChar(_, or)) |
                            (NotChar(_, ol), Range(_, _, or)) |
                            (Range(_, _, ol), AnyChar(or)) |
                            (Range(_, _, ol), Char(_, or)) |
                            (Range(_, _, ol), NotChar(_, or)) |
                            (Range(_, _, ol), Range(_, _, or)) => {
                                i0 = nfa_engine.len();
                                i1 = i0 + 3;
                                *ol = i1;
                                *or = i1;
                                nfa_engine.push(EmptySplit(vec![Out(i0 + 1), Out(i0 + 2)]));
                                nfa_engine.push(s1);
                                nfa_engine.push(s2);
                                nfa_engine.push(EmptySplit(vec![]));
                            }
                            (Fragment(b, e), AnyChar(o)) |
                            (Fragment(b, e), Char(_, o)) |
                            (Fragment(b, e), NotChar(_, o)) |
                            (Fragment(b, e), Range(_, _, o)) => {
                                i0 = nfa_engine.len(); // Index of 1st push to nfa engine
                                match (&nfa_engine[*b], &nfa_engine[*e]) {
                                    (EmptySplit(lv), EmptySplit(rv)) => {
                                        if vec_has_i(lv, *e) || vec_has_i(rv, *b) {
                                            i1 = i0 + 2; // Index of last empty/split
                                            *o = i1;
                                            match &mut nfa_engine[*e] {
                                                AnyChar(oi) | Char(_, oi) | NotChar(_, oi) | Range(_, _, oi) => *oi = i1,
                                                _ => ()
                                            }
                                            nfa_engine.push(EmptySplit(vec![Out(*b), Out(i0 + 1)]));
                                            nfa_engine.push(s2);
                                            nfa_engine.push(EmptySplit(vec![]));
                                        } else {
                                            if let EmptySplit(v) = &mut nfa_engine[*b] {
                                                v.push(Out(i0));
                                                *o = *e;
                                                i0 = *b;
                                                i1 = *e;
                                                nfa_engine.push(s2);
                                            }
                                        }
                                    }
                                    (_, _) => {
                                        i1 = i0 + 2; // Index of last empty/split
                                        *o = i1;
                                        match &mut nfa_engine[*e] {
                                            AnyChar(oi) | Char(_, oi) | NotChar(_, oi) | Range(_, _, oi) => *oi = i1,
                                            _ => ()
                                        }
                                        nfa_engine.push(EmptySplit(vec![Out(*b), Out(i0 + 1)]));
                                        nfa_engine.push(s2);
                                        nfa_engine.push(EmptySplit(vec![]));
                                    }
                                }
                            }
                            (AnyChar(o), Fragment(b, e)) |
                            (Char(_, o), Fragment(b, e)) |
                            (NotChar(_, o), Fragment(b, e)) |
                            (Range(_, _, o), Fragment(b, e)) => {
                                i0 = nfa_engine.len();
                                match (&nfa_engine[*b], &nfa_engine[*e]) {
                                    (EmptySplit(_), EmptySplit(_)) => {
                                        if let EmptySplit(v) = &mut nfa_engine[*b] {
                                            v.push(Out(i0));
                                            *o = *e;
                                            i0 = *b;
                                            i1 = *e;
                                            nfa_engine.push(s1);
                                        }
                                    }
                                    (_, _) => {
                                        i1 = i0 + 2;
                                        *o = i1;
                                        match &mut nfa_engine[*e] {
                                            AnyChar(oi) | Char(_, oi) | NotChar(_, oi) | Range(_, _, oi) => *oi = i1,
                                            _ => ()
                                        }
                                        nfa_engine.push(EmptySplit(vec![Out(*b), Out(i0 + 1)]));
                                        nfa_engine.push(s1);
                                        nfa_engine.push(EmptySplit(vec![]));
                                    }
                                }
                            }
                            (Fragment(bl, el), Fragment(br, er)) => 
                            match (nfa_engine[*bl].clone(), nfa_engine[*el].clone(), nfa_engine[*br].clone(), nfa_engine[*er].clone()) {
                                (EmptySplit(bvl), EmptySplit(evl),
                                EmptySplit(bvr), EmptySplit(evr)) => {
                                    // If LHS and/or RHS fragment have begin bypass to end or, end repeat to begin or any state after begin then
                                    // push 2 new Empty/Split states at begin and end, make them point to 2 fragments begin and fragment pointing to last state.
                                    // Otherwise, Merge RHS fragment begin to LHS fragment begin and delete LHS fragment begin state, do same for RHS end states.
                                    if !vec_has_i(&bvl, *el) && !vec_has_i(&bvr, *er) && !vec_has_i(&evl, *bl) && !vec_has_i(&evr, *br) {
                                        if let EmptySplit(v) = &mut nfa_engine[*bl] {
                                            bvr.into_iter().for_each(|e| { if !v.contains(&e) { v.push(e); }});
                                        }
                                        if let EmptySplit(v) = &mut nfa_engine[*el] {
                                            evr.into_iter().for_each(|e| { if !v.contains(&e) { v.push(e); }});
                                        }
                                        dec_1_if_ge(&mut nfa_engine, *br, *bl);
                                        dec_1_if_ge(&mut nfa_engine, *er, *el);
                                        nfa_engine.remove(*br);
                                        nfa_engine.remove(*er);
                                        i0 = *bl;
                                        i1 = *el;
                                    } else {
                                        i0 = nfa_engine.len();
                                        nfa_engine.push(EmptySplit(vec![Out(*bl), Out(*br)]));
                                        i1 = nfa_engine.len();
                                        nfa_engine.push(EmptySplit(vec![]));
                                        if let EmptySplit(v) = &mut nfa_engine[*el] {
                                            v.push(Out(i1));
                                        }
                                        if let EmptySplit(v) = &mut nfa_engine[*er] {
                                            v.push(Out(i1));
                                        }
                                    }
                                }
                                (EmptySplit(bvl), EmptySplit(evl), _, _) => {
                                    // If begin and end Empty/Split states are not tied in repeat operation, reuse them as begin and end states for RHS fragment.
                                    // Otherwise, push new Empty/Split states with both fragments fall in between.
                                    if bvl.contains(&Out(*el)) || evl.contains(&Out(*bl)) {
                                        //              |--> Fragment1 --v
                                        // Empty/Split -                 Empty/Split
                                        //              |--> Fragment2 --^
                                        i0 = nfa_engine.len();
                                        nfa_engine.push(EmptySplit(vec![Out(*bl), Out(*br)]));
                                        i1 = nfa_engine.len();
                                        nfa_engine.push(EmptySplit(vec![]));
                                        if let EmptySplit(v) = &mut nfa_engine[*el] {
                                            v.push(Out(i1));
                                        }
                                        match &mut nfa_engine[*er] {
                                            AnyChar(o) | Char(_, o) | NotChar(_, o) | Range(_, _, o) => *o = i1,
                                            EmptySplit(v) => v.push(Out(i1)),
                                            _ => ()
                                        }
                                    } else {
                                        // Empty/Split -> ...States... -> Empty/Split
                                        //           \                     ^
                                        //            -->  Fragment 2 -----/
                                        if let EmptySplit(v) = &mut nfa_engine[*bl] {
                                            v.push(Out(*br));
                                        }
                                        match &mut nfa_engine[*er] {
                                            AnyChar(o) | Char(_, o) | NotChar(_, o) | Range(_, _, o) => *o = *el,
                                            EmptySplit(v) => v.push(Out(*el)),
                                            _ => ()
                                        }
                                        i0 = *bl;
                                        i1 = *el;
                                    }
                                }
                                (_, _, EmptySplit(bvr), EmptySplit(evr)) => {
                                    if bvr.contains(&Out(*er)) || evr.contains(&Out(*br)) {
                                        i0 = nfa_engine.len();
                                        nfa_engine.push(EmptySplit(vec![Out(*bl), Out(*br)]));
                                        i1 = nfa_engine.len();
                                        nfa_engine.push(EmptySplit(vec![]));
                                        if let EmptySplit(v) = &mut nfa_engine[*er] {
                                            v.push(Out(i1));
                                        }
                                        match &mut nfa_engine[*el] {
                                            AnyChar(o) | Char(_, o) | NotChar(_, o) | Range(_, _, o) => *o = i1,
                                            EmptySplit(v) => v.push(Out(i1)),
                                            _ => ()
                                        }
                                    } else {
                                        if let EmptySplit(v) = &mut nfa_engine[*br] {
                                            v.push(Out(*bl));
                                        }
                                        match &mut nfa_engine[*el] {
                                            AnyChar(o) | Char(_, o) | NotChar(_, o) | Range(_, _, o) => *o = *er,
                                            EmptySplit(v) => v.push(Out(*er)),
                                            _ => ()
                                        }
                                        i0 = *br;
                                        i1 = *er;
                                    }
                                }
                                (_, _, _, _) => {
                                    // Empty/Split -> (Fragment1 | Fragment2) -> Empty/Split
                                    i0 = nfa_engine.len();
                                    nfa_engine.push(EmptySplit(vec![Out(*bl), Out(*br)]));
                                    i1 = nfa_engine.len();
                                    nfa_engine.push(EmptySplit(vec![]));
                                    match &mut nfa_engine[*el] {
                                        AnyChar(o) | Char(_, o) | NotChar(_, o) | Range(_, _, o) => *o = i1,
                                        _ => ()
                                    }
                                    match &mut nfa_engine[*er] {
                                        AnyChar(o) | Char(_, o) | NotChar(_, o) | Range(_, _, o) => *o = i1,
                                        _ => ()
                                    }
                                }
                            }
                            _ => ()
                        }
                        stack.push(Fragment(i0, i1));
                    }
                    _ => () // End match arm of Oprtr
                }
                _ => ()
            }
        }

        // Pop out only remaining fragment inside stack. Push Start and Match around the fragment.
        // If user regex pattern only contain a single character type, then handle it separetely as [Start -> s1 -> Match]
        // Otherwise, just push start and match as [Start -> {s1(begin) ... s1(end)} -> Match], where content inside {} is not pushed to nfa_engine.
        s1 = stack.pop().unwrap();
        match &mut s1 {
            AnyChar(o) | Char(_, o) | NotChar(_, o) | Range(_, _, o) => {
                i0 = nfa_engine.len() + 1;
                nfa_engine.push(Start(i0));
                *o = i0 + 1;
                nfa_engine.push(s1);
                nfa_engine.push(Match);
            }
            Fragment(b, e) => {
                i0 = nfa_engine.len();
                nfa_engine.push(Start(*b));
                i1 = nfa_engine.len();
                nfa_engine.push(Match);
                match &mut nfa_engine[*e] {
                    AnyChar(o) | Char(_, o) | NotChar(_, o) | Range(_, _, o) => *o = i1,
                    EmptySplit(v) => v.push(Out(i1)),
                    _ => ()
                }
            }
            _ => return Err("Parse Error: Stack contains invalid NFA state type.")
        }

        // Unused state inside temporary stack.
        if !stack.is_empty() {
            return Err("Parse Error: Stack is not empty, contains rouge NFA states.");
        }

        // Invalid state inside nfa engine.
        if nfa_engine.iter().any(|s| { match s{ Oprtr(_) | Fragment(_, _) => true, _ => false }}) {
            return Err("Parse Error: Invalid state type 'Fragment' or 'Operator' in nfa regex engine.");
        }

        Ok(Regex { nfa_engine, start: i0 })
    }

    /// Pretty printing of NFA regex engine states for debug purpose only.
    fn _print_nfa_engine(&self) -> String {
        use State::*;
        use StateOut::*;
        let mut nfa_str = String::from("\nPrinting NFA engine.");
        if self.nfa_engine.is_empty() {
            nfa_str.push_str("Empty NFA state engine.");
        } else {
            nfa_str.push_str(&format!("NFA engine starts at index {}.\n[i]:    <NFA State: {{Char}}| out => NFA State Data>\n", self.start));
            for (i, s) in self.nfa_engine.iter().enumerate() {
                nfa_str.push_str(&format!("[{}]:\t", i));
                match s {
                    AnyChar(o) => nfa_str.push_str(&format!("AnyChar | o => {}\n", o)),
                    Char(c, o) => nfa_str.push_str(&format!("Char: {} | o => {}\n", c, o)),
                    NotChar(c, o) => nfa_str.push_str(&format!("NotChar: {} | o => {}\n", c, o)),
                    Range(b, e, o) => nfa_str.push_str(&format!("Range: {}-{} | o => {}\n", b, e, o)),
                    EmptySplit(v) => {
                        nfa_str.push_str("Empty/Split | o => ");
                        v.iter().for_each(|o| { match o {
                            Out(ao) => nfa_str.push_str(&format!("Out({}), ", ao)),
                            Repeat(ao, m, n, _) => nfa_str.push_str(&format!("Repeat(o={},m={},n={}), ", ao, m, n))
                        }});
                        nfa_str.push('\n');
                    }
                    Start(o) => nfa_str.push_str(&format!("Start | o => {}\n", o)),
                    Match => nfa_str.push_str("Match\n"),
                    _ => ()
                }
            }
        }
        nfa_str
    }

    /// Clears the counter inside repeat state from Empty/Split state's vector for all states inside nfa engine.
    fn clear_repeat_out_counter(&mut self) {
        self.nfa_engine.iter_mut().for_each(|s| {
            if let State::EmptySplit(v) = s {
                v.into_iter().for_each(|e| { if let StateOut::Repeat(_, _, _, c) = e { *c = 0; } });
            }
        });
    }

    /// From the current list of index into nfa engine, gets new list of index of character type states in nfa engine.
    /// Also handles repeat count for repeat type of state.
    fn get_next_states(&mut self, states: &mut Vec<usize>) {
        use State::*;
        use StateOut::*;

        let mut i = 0;
        let mut idx: usize; // index into nfa engine
        let mut size = states.len(); // Size of list of indices into state engine
        let mut current_states = states.clone(); // Update new indices into received state index vector, move input indices to local vector
        states.clear();

        while i < size {
            idx = current_states[i]; // Get index of nfa state engine

            // - If index points to character type state, store index to states.
            // - If index points to Empty/Split, then add all vector elements to current_states; if state out is Repeat(o, m, n, c) then
            //     (where o - nfa engine vec index, m - min repeat count, n - max repeat count, c - current repeat count)
            //   + If c < m, then only add index o from Empty/Split vector.
            //   + If c >= m and c < n, then add index o and other index from Empty/Split vector.
            //   + If c >= n, then skip index o and add every other Out index from Empty/Split vector.
            match &mut self.nfa_engine[idx] {
                AnyChar(_) | Char(_, _) | NotChar(_, _) | Range(_, _, _) | Match => if !states.contains(&idx) { states.push(idx); }
                EmptySplit(v) => {
                    let mut get_other_outs = true;
                    let repeat = v.into_iter()
                        .find_map(|e| {
                            match e { Repeat(_, _, _, _) => Some(e), _ => None }
                        })
                        .and_then(|r| {
                            // Increment the count and verify that if it is less than m, then strict repeat, if less than n then loose repeat, otherwise skip this repeat.
                            match r {
                                Repeat(o, m, n, c) => {
                                    *c += 1;
                                    if *c < *m { get_other_outs = false; Some(*o) }
                                    else if *c >= *m && *c < *n { get_other_outs = true; Some(*o) }
                                    else { get_other_outs = true; None } // We must reset *c if entire match operation fails
                                }
                                _ => { get_other_outs = true; None }
                            }
                        });
                    
                    // If we have the repeat index, do push it in current states
                    if let Some(ro) = repeat {
                        if !current_states.contains(&ro) {
                            current_states.push(ro);
                        }
                    }
                    // If repeat is optional or there is no repeat then, add all out states
                    if get_other_outs {
                        v.iter().for_each(|o| { if let Out(ao) = o {
                            if !current_states.contains(ao) { current_states.push(*ao); }
                        }});
                    }
                    size = current_states.len();
                }
                Start(o) => {
                    // Just append start's out into current states to be searched for character states.
                    if !current_states.contains(o) { current_states.push(*o); }
                    size += 1;
                }
                _ => ()
            }
            i += 1;
        }
    }

    /// Matches given character to character states of nfa state engine for given indices. Returns true if match
    /// if found, false otherwise. Updates the input list of state indices.
    fn match_char(&self, c: char, states: &mut Vec<usize>) -> bool {
        use State::*;

        let mut match_found = false; // True if match state if found, false otherwise
        let current_states = states.clone(); // Clone list of state index values to current temporary state
        let mut next_index: Option<usize> = None; // Temporary storage for next index
        states.clear();

        for si in current_states.iter() {
            // If character state, get its next index. If match state, set the related flag.
            match &self.nfa_engine[*si] {
                AnyChar(o) => if c >= ' ' || c <= '~' { next_index = Some(*o); }
                Char(ch, o) => if c == *ch { next_index = Some(*o); }
                NotChar(ch, o) => if c != *ch { next_index = Some(*o); }
                Range(b, e, o) => if c >= *b && c <= *e  { next_index = Some(*o); }
                Match => match_found = true,
                _ => return false // Any other state is not allowed while character check, its an error, return false
            }

            if let Some(idx) = next_index {
                if !states.contains(&idx) { states.push(idx); }
            }
        }

        match_found
    }

    /// Creates new regex engine given the infix regex pattern.
    /// 
    /// # Errors
    /// Returns descriptive error message string in case of failure which are categorized as:
    /// 1. Syntax Error: Syntax error in the regex pattern.
    ///                  E.g. [[ab]]] (Syntax Error: Nested square brackets '[]' not allowed.)
    /// 2. Parse Error:  Error occured during parsing the postfix regex expression.
    ///                  E.g. [ab]+? (Parse Error: Cannot apply repeat or duplication operator twice.)
    /// 
    /// # Examples
    /// ```
    /// let mut r = nfa_regex::Regex::new("[a-z]").unwrap();
    /// assert_eq!(r.match_pattern("a"), true);
    /// ```
    pub fn new(pattern: &str) -> Result<Self, &'static str> {
        if pattern.is_empty() { return Err("Syntax Error: Input infix pattern string is empty."); }
        let postfix = Regex::infix_to_postfix(pattern)?;
        // println!("{}", Regex::_print_postfix(pattern, &postfix));
        let nfa = Regex::postfix_to_nfa(postfix)?;
        // println!("{}", nfa._print_nfa_engine());
        Ok(nfa)
    }

    /// Matches input string to NFA regex pattern. Returns true if complete string matches the pattern, false otherwise.
    ///
    /// # Examples
    /// ```
    /// let mut r = nfa_regex::Regex::new("[a-z]+").unwrap();
    /// assert_eq!(r.match_pattern("ashutosh"), true);
    /// assert_eq!(r.match_pattern("[om, ashutosh]"), false);
    /// assert_eq!(r.match_pattern(""), false);
    /// ```
    pub fn match_pattern(&mut self, s: &str) -> bool {
        self.clear_repeat_out_counter(); // Clear any count values inside repeat outs of empty/split state
        let mut states = vec![self.start]; // Start fresh with start state
        let mut match_found = false; // True if match state is found, false otherwise
        for c in s.chars() {
            self.get_next_states(&mut states);
            match_found = self.match_char(c, &mut states);
            if states.is_empty() {
                // If state is empty, it means the input string does not match perfectly to our regex pattern
                match_found = false;
                break;
            }
        }
        if !states.is_empty() {
            // If states available to check, then only check for match state as we already matched all characters in input string to pattern and they are perfect match till now.
            self.get_next_states(&mut states);
            match_found = states.iter().any(|i| { &self.nfa_engine[*i] == &State::Match });
        }

        match_found
    }
    
    /// Searches the pattern inside string and returns the first match's begin and end index into the input string, otherwise returns begin and end index as 0.
    ///
    /// # Examples
    /// ```
    /// let mut r = nfa_regex::Regex::new("[a-z]+").unwrap();
    /// assert_eq!(r.search_pattern_and_get_index("ashutosh"), (0, 8));
    /// assert_eq!(r.search_pattern_and_get_index("[om, ashutosh, 3.141593]"), (1, 3));
    /// assert_eq!(r.search_pattern_and_get_index(""), (0, 0));
    /// ```
    pub fn search_pattern_and_get_index(&mut self, s: &str) -> (usize, usize) {
        self.clear_repeat_out_counter(); // Clear any count values inside repeat outs of empty/split state
        let mut c: char; // Char of input string
        let mut states = vec![self.start]; // Start from start state
        let mut match_begin = None; // The begin index of pattern match
        let mut match_end = None; // The end index of pattern match
        let mut match_found = false; // True if match state is found, false otherwise
        let mut i = 0; // Index into input string
        // In case pattern match fails but string is still not completely searched for, then restart the search from begin + 1 index since begin index character was already checked and failed.
        while i < s.len() {
            c = s.chars().nth(i).unwrap();
            self.get_next_states(&mut states);
            match_found = self.match_char(c, &mut states);
            if states.is_empty() {
                // If state indices is empty, then if match found and we have valid match begin, then get current index into string as end index.
                // Otherwise, restart search from index begin + 1, since search from begin index already failed, so restart from its next character all over again
                if match_found && match_begin != None {
                    match_end = Some(i);
                    break;
                } else {
                    if let Some(k) = match_begin {
                        // Not incrementing here as we already have increment at the end of loop to begin search from begin + 1.
                        i = k;
                    }
                    match_begin = None;
                    match_end = None;
                    states.push(self.start);
                }
            } else {
                // If we have states, then its a match of first character, use current index as begin index
                if match_begin == None {
                    match_begin = Some(i);
                }
            }
            i += 1;
        }

        // If match not found, then
        if !match_found {
            // And next states is empty
            if states.is_empty() {
                // Reset match indices
                match_begin = None;
                match_end = None;
            } else {
                // Otherwise, get next states, and try to find match state.
                self.get_next_states(&mut states);
                // If found, break and return matched sub-string.
                for i in states {
                    match &self.nfa_engine[i] {
                        State::Match => { match_end = Some(s.len()); break; }
                        _ => ()
                    }
                }
            }
        } else {
            // Match state found, match pattern begin index is valid, then we have last character as match pattern end.
            if match_begin != None && match_end == None {
                match_end = Some(s.len());
            }
        }

        // We have valid begin and end matched pattern indices, then get and return prefix, suffix and macthed string slices.
        if let (Some(begin), Some(end)) = (match_begin, match_end) {
            if begin < end && end <= s.len() {
                return (begin, end);
            }
        }

        (0, 0)
    }

    /// Search the substring inside given string which matches the regex pattern and return unmatched prefix, matched and unmatched suffix string slices.
    ///
    /// # Examples
    /// ```
    /// let mut r = nfa_regex::Regex::new("[a-z]+").unwrap();
    /// assert_eq!(r.search_pattern_and_get_slice("ashutosh"), ("", "ashutosh", ""));
    /// assert_eq!(r.search_pattern_and_get_slice("[om, ashutosh, 3.141593]"), ("[", "om", ", ashutosh, 3.141593]"));
    /// assert_eq!(r.search_pattern_and_get_slice(""), ("", "", ""));
    /// ```
    pub fn search_pattern_and_get_slice<'a>(&mut self, s: &'a str) -> (&'a str, &'a str, &'a str) {
        let (begin, end) = self.search_pattern_and_get_index(s);
        (&s[0..begin], &s[begin..end], &s[end..])
    }

    /// Search all substrings inside given string which matches regex pattern. Return the list of begin-end indices for input string of all matched pattern.
    ///
    /// # Examples
    /// ```
    /// let mut r = nfa_regex::Regex::new("[a-z]+").unwrap();
    /// assert_eq!(r.search_all_patterns_and_get_index_list("ashutosh"), vec![(0, 8)]);
    /// assert_eq!(r.search_all_patterns_and_get_index_list("[om, ashutosh, 3.141593]"), vec![(1, 3), (5, 13)]);
    /// assert_eq!(r.search_all_patterns_and_get_index_list(""), vec![]);
    /// ```
    pub fn search_all_patterns_and_get_index_list(&mut self, s: &str) -> Vec<(usize, usize)> {
        let mut index_list: Vec<(usize, usize)> = vec![];
        let mut end = 0;
        // Every time we get the substring match, we strip the string slice begging till match end index, hence using end index to accumulate all end index values so always get current index into input string.
        loop {
            let (b, e) = self.search_pattern_and_get_index(&s[end..]); // Get begin and end index.
            if (b == 0 && e == 0) || end >= s.len() { break; } // Make sure they are valid
            index_list.push((b + end, e + end)); // Push these indices to vector
            end += e; // Update end index with input e index.
        }
        index_list
    }

    /// Replace the occurance of first match of regex pattern to substring inside given input string with another given source string.
    ///
    /// # Examples
    /// ```
    /// let mut r = nfa_regex::Regex::new("[a-z]+").unwrap();
    /// let mut s1 = String::from("ashutosh");
    /// let mut s2 = String::from("[om, ashutosh, 3.141593]");
    /// r.replace_pattern(&mut s1, "MAHAKALI");
    /// r.replace_pattern(&mut s2, "MAHAKALI");
    /// assert_eq!(s1, "MAHAKALI");
    /// assert_eq!(s2, "[MAHAKALI, ashutosh, 3.141593]");
    /// ```
    pub fn replace_pattern(&mut self, target: &mut String, src: &str) {
        let (begin, end) = self.search_pattern_and_get_index(target);
        if begin < end && end <= target.len() {
            target.replace_range(begin..end, src);
        }
    }

    /// Replaces all occurances of substring matching regex pattern inside given input string with given source string.
    /// Returns unchanged string if no matched substring found.
    ///
    /// # Examples
    /// ```
    /// let mut r = nfa_regex::Regex::new("[a-z]+").unwrap();
    /// let mut s = String::from("[om, ashutosh, 3.141593]");
    /// r.replace_all_patterns(&mut s, "MAHAKALI");
    /// assert_eq!(s, "[MAHAKALI, MAHAKALI, 3.141593]");
    /// ```
    pub fn replace_all_patterns(&mut self, target: &mut String, src: &str) {
        let i = self.search_all_patterns_and_get_index_list(target);
        i.into_iter().rev().for_each(|(b, e)| { target.replace_range(b..e, src); } );
    }
}


#[cfg(test)]
mod tests {
    use crate::Regex;
    #[test]
    fn regex_function_test() {
        let mut r = Regex::new("[a-z]+").unwrap();
        assert_eq!(r.match_pattern("ashutosh"), true);
        assert_eq!(r.match_pattern(""), false);

        // A regex pattern for real number types
        let mut r = Regex::new("[\\+\\-]?[0-9]+(\\.[0-9]+)?([eE][\\+\\-]?[0-9]+)?").unwrap();
        assert_eq!(r.match_pattern("-0.223654E-9"), true);
        assert_eq!(r.search_pattern_and_get_index(""), (0, 0));
        assert_eq!(r.search_pattern_and_get_slice(""), ("", "", ""));
        assert_eq!(r.search_pattern_and_get_slice("a = 0.2232+3.2i"), ("a = ", "0.2232", "+3.2i"));
        assert_eq!(r.search_pattern_and_get_index("Income tax slab of 30.5%."), (19, 23));
        assert_eq!(r.search_all_patterns_and_get_index_list("pi = 3.141593, x = -2.12e-10"), vec![(5, 13), (19, 28)]);

        let mut s = String::from("A picture worth 1000 words.");
        r.replace_pattern(&mut s, "some");
        assert_eq!(s, "A picture worth some words.");
        r.replace_pattern(&mut s, "src");
        assert_eq!(s, "A picture worth some words.");

        s = String::from("pi = 3.141593, x = -2.12e-10, DONE.");
        r.replace_all_patterns(&mut s, "Some Number");
        assert_eq!(s, "pi = Some Number, x = Some Number, DONE.");

        let mut r = Regex::new("[a#b]+").unwrap();
        assert_eq!(r.match_pattern("#b"), true);
    }
}
