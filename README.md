# rmonkey
rmonkey is a tiny interpreter written in Rust.  

# HowToUse
```
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
     Running `target/debug/rmonkey`
>> 
let add = fn(a, b) {a + b};
`null
>> 
let sub = fn(a, b) {a - b};
`null
>> 
let applyfunc = fn(a, b, func) { func(a, b)};
`null
>> 
applyfunc(2, 2, add);
`4
>> 
```

# Referrence
https://interpreterbook.com/

# GO-like super dirty code branch is here!
[branch](https://github.com/morimolymoly/rmonkey/tree/update)

It uses many shitty traits and structs and Any and downcast shit.
