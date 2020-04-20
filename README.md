# rmonkey
[![Build Status](https://travis-ci.org/morimolymoly/rmonkey.svg?branch=master)](https://travis-ci.org/morimolymoly/rmonkey)  
rmonkey is a tiny interpreter written in Rust.  

# HowToUse
```
$ cargo run
   Compiling rmonkey v0.1.0 (/Users/mizuho/work/rust/rmonkey)
    Finished dev [unoptimized + debuginfo] target(s) in 0.94s
     Running `target/debug/rmonkey`
>> let add = fn(a, b) {a + b};
null
>> let sub = fn(a, b) {a - b};
null
>> let applyfunc = fn(a, b, func) { func(a, b)};
null
>> applyfunc(2, 2, add);
4
>> "hello" + ", world!";
"hello, world!"
>> len("")
0
>> len("hello")
5
>> len(1)
ERROR: argument to 'len' not supported, got=INTEGER
>> len("a", "b");
ERROR: wrong number of arguments, got=2
>> len([])
0
>> len([10, 20, 30])
3
>> stoi("19");
19
>> let mya = [10, 20, 30, 40];
null
>> mya
[10, 20, 30, 40]
>> first(mya)
10
>> last(mya)
40
>> rest(mya)
[20, 30, 40]
>> push(mya, "fjakdfjl")
[10, 20, 30, 40, fjakdfjl]
>> mya
[10, 20, 30, 40]
>> let aaa = fn(x){if(x>0){dbg_print();aaa(x-1)}else{0}};
null
>> aaa(3);
dbg!
dbg!
dbg!
0
>> let hash = {true: 1, "test": 2, false: 3, 4: 4};   
null
>> hash[true]
1
>> hash[false]
3
>> hash[1==1]
1
>> hash["test"]
2
>> hash[4]
4
>> let unless = macro(condition, consequence, alternative) {quote(if (!(unquote(condition))) {unquote(consequence);} else {unquote(alternative);});};
null
>> unless(10>5, puts("not greater"), puts("greater"));
greater
debug function
```

# Referrence
* https://interpreterbook.com/
* https://github.com/kogai/monkey

# GO-like super dirty code branch is here!
[branch](https://github.com/morimolymoly/rmonkey/tree/update)

It uses many shitty traits and structs and Any and downcast shit.
