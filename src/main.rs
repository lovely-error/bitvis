
use core::{fmt::Write, iter::zip};
use std::{env, io::{prelude::Write as _, stderr, stdout}};

enum Dir { LTR, RTL }

#[derive(Clone)]
struct Seg {
    width: usize,
    name: Option<String>,
    binary_pattern: Option<String>
}
struct Item {
    comps: Vec<Seg>,
    direction: Dir
}

fn render_pat(pat: &Item, str: &mut String) {

    let Item { comps, direction } = pat;
    let mut comps = comps.clone();
    // let mut forced_width = None;
    // for attr in inp.attrs {
    //     match attr {
    //         PatAttr::FW(fw) => { forced_width = Some(fw); break }
    //     }
    // }
    let mut bit_width = 0;

    for item in &comps {
        bit_width += item.width;
    }
    let max_bit_index_width =
        if bit_width < 2 { 1 }
        else { (bit_width as f32).log(9.0).ceil() as usize };

    let anti ;
    match direction {
        Dir::LTR => {
            anti = bit_width - 1;
        },
        Dir::RTL => {
            comps.reverse();
            anti = 0;
        },
    }

    let mut tmp = String::new();

    let mut line_sep = String::new();
    tmp.push('+');
    tmp.push_str("--");
    for _ in 0 .. max_bit_index_width {
        tmp.push('-');
    }
    for _ in 0 .. bit_width {
        line_sep.push_str(&tmp);
    }
    line_sep.push('+');
    tmp.clear();


    let mut ix = 1 - (bit_width as isize) ;
    loop {
        let bit_index = (ix + anti as isize).abs() as usize;

        str.push(' ');
        str.push_str(" ");
        let lpad = max_bit_index_width -
            if bit_index < 2 { 1 }
            else { (bit_index as f32).log(9.0).ceil() as usize };
        for _ in 0 .. lpad {
            tmp.push(' ');
        }
        write!(&mut tmp, "{}", bit_index).unwrap();
        str.push_str(&tmp);
        tmp.clear();
        str.push_str(" ");
        if bit_index == anti {
            break;
        }
        ix += 1;
    }
    str.push('\n');
    str.push_str(&line_sep);
    str.push('\n');

    tmp.push_str("  ");
    for _ in 0 .. max_bit_index_width {
        tmp.push(' ');
    }
    for comp in comps {
        str.push('|');
        let mut ix = comp.width;
        match (&comp.binary_pattern, &comp.name) {
            (Some(pat), None) => {
                assert!(pat.len() <= comp.width);
                let pad = max_bit_index_width - 1;
                let len = pat.len();
                let mut ix = len - 1;
                let mut ptr = pat.as_ptr();
                loop {
                    str.push(' ');
                    for _ in 0 .. pad {
                        str.push(' ');
                    }
                    let char = unsafe { *ptr } as char;
                    write!(str, "{}", char).unwrap();
                    str.push(' ');
                    if ix == 0 { break }
                    str.push(' ');
                    ix -= 1;
                    ptr = unsafe { ptr.add(1) };
                }
                if len < comp.width {
                    let mut miss = comp.width - len;
                    str.push(' ');
                    loop {
                        str.push(' ');
                        for _ in 0 .. max_bit_index_width {
                            str.push('_');
                        }
                        str.push(' ');
                        if miss == 1 { break }
                        str.push(' ');
                        miss -= 1;
                    }
                }
            },
            (None, Some(name)) => {
                let available_width = (comp.width * (max_bit_index_width + 2)) + comp.width - 1;
                let len = name.len();
                let fits = available_width > len + 2;
                if fits {
                    let padl = (((available_width - len) as f32) / 2.0).ceil() as usize;
                    for _ in 0 .. padl {
                        str.push(' ');
                    }
                    str.push_str(name);
                    let padr = available_width - (padl + len);
                    for _ in 0 .. padr {
                        str.push(' ');
                    }
                } else {
                    let can_fit = available_width - 2;
                    let mut ix = 0;
                    let name = name.as_bytes();
                    str.push(' ');
                    loop {
                        if ix == can_fit { break }
                        if ix + 1 == can_fit {
                            str.push('>');
                            break;
                        }
                        str.push(name[ix] as char);
                        ix += 1;
                    }
                    str.push(' ');
                }
            },
            (Some(_), Some(_)) => todo!(),
            (None, None) => loop {
                str.push_str(&tmp);
                if ix == 1 { break }
                str.push(' ');
                ix -= 1;
            },
        }
    }
    tmp.clear();
    str.push_str("|\n");
    str.push_str(&line_sep);
}

#[derive(Debug)]
enum Failure {
    Msg(String)
}


struct Parser<'a> {
    chars: &'a [u8],
    index: usize
}
impl<'i> Parser<'i> {
    fn new(chars: &'i [u8]) -> Self {
        Self { chars, index: 0 }
    }
    fn skip_while(&mut self, mut cond: impl FnMut(u8) -> bool) -> (usize, usize) {
        let mut ix = self.index;
        loop {
            if ix >= self.chars.len() {
                break
            }
            let item = self.chars[ix];
            if cond(item) { ix += 1; } else { break }
        }
        let rv = (self.index, ix);
        self.index = ix;
        return rv;
    }
    fn skip_trivia(&mut self) {
        let _ = self.skip_while(|c| c.is_ascii_whitespace());
    }
    fn skip(&mut self, count:usize) {
        if self.index >= self.chars.len() {
            return;
        }
        self.index += count;
    }
    fn current_char(&self) -> char {
        let ix = self.index ;
        if ix >= self.chars.len() {
            return '\0';
        }
        return self.chars[ix] as char
    }
    fn try_skip_exact(&mut self, pat: &str) -> bool {
        let mut ix = self.index ;
        let mut ok = true;
        let mut iter = pat.chars().into_iter();
        loop {
            if ix >= self.chars.len() {
                ok = false; break;
            }
            if let Some(thing) = iter.next() {
                let char = self.chars[ix] as char;
                if thing != char { ok = false ; break };
                ix += 1;
            } else {
                break
            };
        };
        if ok { self.index = ix as _ };
        return ok;
    }
    fn mk_error(&self, arg: String) -> String {
        let ix = self.index;
        let ch = self.chars[ix] as char;
        format!(
            "| !!! Malformed input !!!\n| Parsing stop at character {} which is `{}` (codepoint {})\n| Reason: {}",
            ix, ch, ch.escape_unicode(), arg)
    }
    fn try_take_number(&mut self) -> Result<usize, Failure> {
        let (s, e) = self.skip_while(|c|c.is_ascii_digit());
        if s == e {
            return Err(Failure::Msg(self.mk_error("Expected number".to_string())));
        }
        let sl = &self.chars[s..e];
        let str = match core::str::from_utf8(sl) {
            Ok(v) => v,
            Err(err) => {
                return Err(Failure::Msg(format!("{}", err)));
            },
        };
        let num: usize = match str.parse() {
            Ok(v) => v,
            Err(err) => {
                return Err(Failure::Msg(format!("{}", err)));
            },
        };
        Ok(num)
    }
    fn is_valid_text(ch:char) -> bool {
        ch.is_ascii_alphanumeric() || (ch as char == '_')
    }
    fn parse_seg(&mut self) -> Result<Seg, Failure> {
        let width = self.try_take_number()?;
        self.skip_trivia();
        let mut bp = None;
        if self.try_skip_exact("#") {
            let (s, e) = self.skip_while(|c|c.is_ascii_alphanumeric());
            if s == e {
                return Err(Failure::Msg(self.mk_error("Expected number , got nothing".to_string())));
            }
            let fp = match core::str::from_utf8(&self.chars[s..e]) {
                Ok(v) => v.to_string(),
                Err(err) => return Err(Failure::Msg(err.to_string())),
            };
            bp = Some(fp);
        }
        self.skip_trivia();
        let mut txt = None;
        if Self::is_valid_text(self.current_char()) {
            let (s, e) = self.skip_while(|c|Self::is_valid_text(c as char));
            if s == e {
                return Err(Failure::Msg(self.mk_error("Expected text , got nothing".to_string())));
            }
            let txt_ = match core::str::from_utf8(&self.chars[s..e]) {
                Ok(v) => v.to_string(),
                Err(err) => return Err(Failure::Msg(err.to_string())),
            };
            txt = Some(txt_)
        }
        let v = Seg { width, binary_pattern: bp, name: txt };
        return Ok(v);
    }
    fn parse_item(&mut self) -> Result<Item, Failure> {
        self.skip_trivia();
        let dir = match self.current_char() {
            '>' => Dir::LTR,
            '<' => Dir::RTL,
            _ => {
                return Err(Failure::Msg(self.mk_error("Expected > or < ".to_string())));
            }
        };
        self.skip(1);
        self.skip_trivia();
        if !self.try_skip_exact("|") {
            return Err(Failure::Msg(self.mk_error("Expected |".to_string())));
        };
        let mut comps = Vec::new();
        loop {
            self.skip_trivia();
            let seg = self.parse_seg()?;
            comps.push(seg);
            self.skip_trivia();
            if self.try_skip_exact(",") {
                continue;
            }
            break;
        }
        for comp in &comps {
            if let (Some(name), Some(pattern)) = (&comp.name, &comp.binary_pattern) {
                return Err(Failure::Msg(format!(
                    "One clause has both name ({}) and bitpattern ({}). This is not supported yet :(",
                    name, pattern)));
            }
        }
        let v = Item {
            comps,
            direction: dir
        };
        return Ok(v);
    }
    fn parse_items(&mut self) -> Result<Vec<Item>, Failure> {
        self.skip_trivia();
        let mut items = Vec::new();
        loop {
            let item = self.parse_item()?;
            items.push(item);
            self.skip_trivia();
            if self.try_skip_exact(";") {
                continue;
            };
            break;
        }
        let ix = self.index;
        let len = self.chars.len();
        if ix < len {
            return Err(Failure::Msg(self.mk_error(format!("{} more chracters got ignored!", len - ix))))
        }
        return Ok(items);
    }
}

fn main() {

    let mut stderr = stderr();

    let inp = env::args().collect::<Vec<String>>();
    if inp.len() != 2 {
        write!(&mut stderr, "Unexpected arguments. Need a pattern").unwrap();
        return;
    }

    let inp = &inp[1];

    if !inp.is_ascii() {
        write!(&mut stderr, "Non-ascii input is not yet supported :(").unwrap();
        return;
    }
    let s = unsafe { core::slice::from_raw_parts(inp.as_ptr(), inp.len()) };
    let mut par = Parser::new(s);
    let items = match par.parse_items() {
        Ok(v) => v,
        Err(err) => {
            match err {
                Failure::Msg(msg) =>
                    write!(&mut stderr, "{}", msg).unwrap()
            }
            return;
        },
    };

    for (item_ix, item) in zip(1.., &items) {
        for (seg_ix, comp) in zip(0.., &item.comps) {
            let bitwidth = comp.width;
            if let Some(bpat) = &comp.binary_pattern {
                let pat_len = bpat.len();
                if pat_len  > bitwidth {
                    write!(&mut stderr,
                        "Invalid configuration in item {} in segment {}. Field width is {}, but pattern length is {}",
                        item_ix, seg_ix, bitwidth, pat_len).unwrap();
                    return;
                }
            }
            if bitwidth == 0 {
                write!(&mut stderr, "Zero bitwidth in item {} in segment {}", item_ix, seg_ix).unwrap();
            }
        }
    }

    let mut str = String::new();
    for item in items {
        render_pat(&item, &mut str);
        str.push_str("\n\n");
    }

    let mut stdout = stdout();

    stdout.write(str.as_bytes()).unwrap();

}

#[test]
fn hui() {
    println!("{}", (1 as f32).log(9.0).ceil())
}

#[test]
fn hui3() {

    let mut ix = -8i32 + 1;
    let c = if true { 8 - 1 } else { 0 };
    loop {
        let i = (ix + c).abs();
        println!("{}", i);
        if i == c { break }
        ix += 1;
    }
}

#[test]
fn hui4() {

    let str = "< | 2 #01, 3 #011, 2 DT, 3 DST, 6 SRC";
    let mut par = Parser::new(str.as_bytes());
    let items = match par.parse_items() {
        Ok(v) => v,
        Err(err) => {
            match err {
                Failure::Msg(msg) =>
                    println!("{}", msg)
            }
            return;
        },
    };

    for (item_ix, item) in zip(1.., &items) {
        for (seg_ix, comp) in zip(0.., &item.comps) {
            if let Some(bpat) = &comp.binary_pattern {
                let bitwidth = comp.width;
                let pat_len = bpat.len();
                if pat_len  > bitwidth {
                    println!("Invalid configuration in item {} in segment {}. Field width is {}, but pattern length is {}",
                        item_ix, seg_ix, bitwidth, pat_len);
                    return;
                }
            }
        }
    }

    let mut str = String::new();
    render_pat(&items[0], &mut str);

    println!("{}", str)
}

