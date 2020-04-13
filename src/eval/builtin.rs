use crate::object;

pub const ERR_BUILT_IN_LEN_ARG_SUPPORT: &'static str = "argument to 'len' not supported, got=";
pub const ERR_BUILT_IN_ARG_NUM: &'static str = "wrong number of arguments, got=";
pub const ERR_BUILT_IN_STOI_ARG_SUPPORT: &'static str = "argument to 'stoi' not supported, got=";
pub const ERR_BUILT_IN_ARRAY_INDEX_OUT_OF_RANGE: &'static str = "index out of range";
pub const ERR_BUILT_IN_FIRST_ARG_SUPPORT: &'static str = "argument to 'first' not supported, got=";
pub const ERR_BUILT_IN_LAST_ARG_SUPPORT: &'static str = "argument to 'last' not supported, got=";
pub const ERR_BUILT_IN_REST_ARG_SUPPORT: &'static str = "argument to 'rest' not supported, got=";
pub const ERR_BUILT_IN_PUSH_ARG_SUPPORT: &'static str = "argument to 'rest' not supported, got=";

pub fn builtin_len_function(arg: Vec<object::Object>) -> object::Object {
    if arg.len() != 1 {
        return object::Object::Error(format!("{}{}", ERR_BUILT_IN_ARG_NUM, arg.len()));
    }
    let arg = arg[0].clone();
    match arg {
        object::Object::String(s) => return object::Object::Integer(s.len() as i64),
        object::Object::Array(v) => return object::Object::Integer(v.len() as i64),
        _ => {
            return object::Object::Error(format!(
                "{}{}",
                ERR_BUILT_IN_LEN_ARG_SUPPORT,
                arg.mytype()
            ))
        }
    }
}

pub fn builtin_stoi_function(arg: Vec<object::Object>) -> object::Object {
    if arg.len() != 1 {
        return object::Object::Error(format!("{}{}", ERR_BUILT_IN_ARG_NUM, arg.len()));
    }

    let arg = arg[0].clone();
    match arg {
        object::Object::String(s) => match s.parse() {
            Ok(d) => object::Object::Integer(d),
            Err(e) => object::Object::Error(e.to_string()),
        },
        _ => {
            return object::Object::Error(format!(
                "{}{}",
                ERR_BUILT_IN_LEN_ARG_SUPPORT,
                arg.mytype()
            ))
        }
    }
}

pub fn builtin_first_function(arg: Vec<object::Object>) -> object::Object {
    if arg.len() != 1 {
        return object::Object::Error(format!("{}{}", ERR_BUILT_IN_ARG_NUM, arg.len()));
    }
    let arg = arg[0].clone();
    match arg {
        object::Object::Array(v) => {
            if v.len() == 0 {
                return object::Object::Error(format!("{}", ERR_BUILT_IN_ARRAY_INDEX_OUT_OF_RANGE));
            }
            return *v[0].clone();
        }
        _ => {
            return object::Object::Error(format!(
                "{} {}",
                ERR_BUILT_IN_FIRST_ARG_SUPPORT,
                arg.mytype()
            ))
        }
    }
}

pub fn builtin_last_function(arg: Vec<object::Object>) -> object::Object {
    if arg.len() != 1 {
        return object::Object::Error(format!("{}{}", ERR_BUILT_IN_ARG_NUM, arg.len()));
    }
    let arg = arg[0].clone();
    match arg {
        object::Object::Array(v) => {
            if v.len() == 0 {
                return object::Object::Error(format!("{}", ERR_BUILT_IN_ARRAY_INDEX_OUT_OF_RANGE));
            }
            return *v[v.len() - 1].clone();
        }
        _ => {
            return object::Object::Error(format!(
                "{} {}",
                ERR_BUILT_IN_LAST_ARG_SUPPORT,
                arg.mytype()
            ))
        }
    }
}

pub fn builtin_rest_function(arg: Vec<object::Object>) -> object::Object {
    if arg.len() != 1 {
        return object::Object::Error(format!("{}{}", ERR_BUILT_IN_ARG_NUM, arg.len()));
    }
    let arg = arg[0].clone();
    match arg {
        object::Object::Array(v) => {
            let mut newa = v.clone();
            if v.len() == 0 {
                return object::Object::Array(newa);
            }
            newa.remove(0);
            return object::Object::Array(newa);
        }
        _ => {
            return object::Object::Error(format!(
                "{} {}",
                ERR_BUILT_IN_LAST_ARG_SUPPORT,
                arg.mytype()
            ))
        }
    }
}

pub fn builtin_push_function(arg: Vec<object::Object>) -> object::Object {
    if arg.len() != 2 {
        return object::Object::Error(format!("{}{}", ERR_BUILT_IN_ARG_NUM, arg.len()));
    }
    let vector = arg[0].clone();
    let value = arg[1].clone();
    match vector {
        object::Object::Array(v) => {
            let mut newa = v.clone();
            newa.push(Box::new(value));
            return object::Object::Array(newa);
        }
        _ => {
            return object::Object::Error(format!(
                "{} {}",
                ERR_BUILT_IN_PUSH_ARG_SUPPORT,
                vector.mytype()
            ))
        }
    }
}
