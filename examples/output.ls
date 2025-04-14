
// struct literals
r1 = Vector2.{ 3, 4 } * 5;
r1 -= .{ 3, 2 };

// operators and procedure calls
r2 =(1 + 2) * pow(2, sin(3.14159/2) + 2);
r3 = a + b;

// while loops
while echo(r2) > 20 {
   r2 = r2 - 1;
   r3 = r3 + r3;
}


// for loops and casting
for arr1  if it > 1  it = it_index.(float) * 7;
for arr1  echo(it);

// local declarations
local: float;
local = 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5;
echo(local/2);

// assignment operators redux
local = 2;
local += 5;
echo(local);

// range-based for loops and unary operators
for -5..9  echo(it);

// strings
print(str);
print("this is also a string");

local_str := "this is yet another string";
print(local_str);
str = local_str;


// Directives Test
// #declare will call add_named_value in test.jai, which will add a new Named_Value to the array and add that value as a variable in the script.
// This allows the script to use 'named_1' as a simple variable although its storage is managed externally.
#declare("named_1");
named_1 = 7;
echo(named_1);


// struct literal and operator overload test
vec3 := Vector3.{ 1, 2, 3 } * 7;
echo(vec3);


// malleable literals
// you will notice that the value of 5 in the declaration for const will be overwritten with 7 when this file is re-serialized (look at output.ls)
// also, if you were to execute the script multiple times, it would only be 5 on the first run
const :: 7?;
echo(const);

const = 7;
echo(const);


// backticked identifier strings test
`identifier string` := 69;
echo(`identifier string`);


// identifier resolution callbacks / node replacement test
// cool_number will be replaced with '35' when re-serialized
// node replacement is not really something to be used if one plans to re-serialize their ast later, since it's inherently a destructive transformation.
// but, for throwaway scripts or expressions it can be very handy
// asdf := cool_number;
// echo(asdf);
// echo(cool_number);


e := Fruits.BANANA;
echo(e);
e = .PEAR;
echo(e);

// test of better procedure overload resolution
echo(length(.{1, 2 } + .{2, 2 }));
echo(f32(5)); // stupid and confusing example
{
   a: int = 5;
   b: u8 = 3; 
   echo(a);
   echo(b);
   a = b;
   echo(a);
}


// test of implicit reference semantics for procedure arguments
// v2 is taken by reference in call to 'scale'
v2 := Vector2.{ 1, 2 };
scale(v2, 3);
echo(v2);

// comma separated lists for iteration 
// this is sort of a cope for not having array literals
{
   a := 5;
   b := 96;
   c := -8;
   foreach(a, b, c)  echo(it);
}


some_value := #add(1, 2);
echo(some_value);

// named blocks
test_block: {
   print("Running test block.");
}
