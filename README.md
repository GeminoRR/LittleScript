# LittleScript
Little Script is a scripting language based on the lexer and the AST of [lim](http://github.com/geminorr/lim) to test its portability.

## Main feature
- Simple and easy to understand syntax
- An automatic conversion of the types of values according to the operator used.


## Examples
### Hello world
```ruby
msg "Hello world"
```

### If statement
```ruby
ask "Enter your age : " to age
if age > 18 do
	msg "Hello !"
else do
	msg "Hi !"
```

### Loop
```ruby
for i in range(20) do
	msg "Loop " & i
```

### Fibonacci
```ruby
fun fibo
	set result to []
	set num1 to 0
	set num2 to 1
	for i in range(20) do
		set temp to num1 + num2
		set num1 to num2
		set num2 to temp
		append temp to result
	return result

msg fibo()
```