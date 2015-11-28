# "VM5294" virtual machine in Haskell

This is a toy multi-core virtual machine which executes instructions
such as those in the `examples/*.a` files. The code originated as an
internal Ruby coding challenge for our developers at
[Powershop](http://www.powershop.com/culture/) -- I couldn't resist
trying it in Haskell too.

## Usage

```
> stack setup && stack install
> vm5294 examples/minimum_dualproc.a
... enter pairs of numbers followed by newlines ...
```
