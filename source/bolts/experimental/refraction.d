/++
This module helps building functions from other functions.

 It is sometimes necessary to create a function which is an exact copy of
 another function. Or sometimes it is necessary to introduce a few variations,
 while carrying all the other aspects. Because of function attributes,
 parameter storage classes and user-defined attributes, this requires building
 a string mixin. In addition, the mixed-in code must refer only to local names,
 if it is to work across module boundaires. This problem and its solution are
 by Adam D. Ruppe in a Tip of the Week, available here:
 https://stackoverflow.com/questions/32615733/struct-composition-with-mixin-and-templates/32621854#32621854

 This module facilitates the  creation of such mixins.

 +/

module bolts.experimental.refraction;

import std.array;
import std.format;
import std.meta;
import std.algorithm.iteration : map;
import std.range : iota;
import std.traits : functionAttributes, FunctionAttribute;

// Do not require caller module to import 'std.traits'. Instead use our own
// aliases in mixtures.
alias ReturnType = std.traits.ReturnType;
alias Parameters = std.traits.Parameters;

template ParameterAttribute(alias F, int i, int j) {
    static if (is(typeof(F) P == __parameters)) {
        alias ParameterAttribute = Alias!(__traits(getAttributes, P[i..i+1])[j]);
    }
}

unittest {
    struct virtual;
    void kick(int times, @virtual @("Animal") Object animal);
    static assert(is(ParameterAttribute!(kick, 1, 0) == virtual));
    static assert(ParameterAttribute!(kick, 1, 1) == "Animal");
}

/**
   Return a `Function` object that captures all the aspects of `fun`, using the
   value of `localName` to represent the return and parameter types, and the
   UDAs.

   The `localName` parameter is, in general, *not* the function name. Rather,
   it is a compile-time expression that involves only symbols that exist in the
   caller's scope, for example a function alias passed as a template
   parameter. See
   https://stackoverflow.com/questions/32615733/struct-composition-with-mixin-and-templates/32621854#32621854
   for a detailed explanation.

   Params:
   fun = a function
   localName = a string that represents `fun` in the caller's context
*/

Function refract(alias fun, string localName)()
if (is(typeof(fun) == function)) {
    Function model = {
    name: __traits(identifier, fun),
    index: -1,
    localName: localName,
    returnType: "bolts.experimental.refraction.ReturnType!("~localName~")",
    parameters: refractParameterList!(fun, localName),
    udas: __traits(getAttributes, fun)
    .length.iota.map!(
        formatIndex!("@(__traits(getAttributes, %s)[%%d])".format(localName))).array,
    attributes: functionAttributes!(fun),
    static_: __traits(isStaticFunction, fun) && isAggregate!(__traits(parent, fun)),
    body_: ";",
    };

    return model;
 }

///
unittest {
    pure @nogc int answer(lazy string question);
    alias F = answer; // typically F is a template argument
    static assert(
        refract!(F, "F").mixture ==
        "pure @nogc @system bolts.experimental.refraction.ReturnType!(F) answer(lazy bolts.experimental.refraction.Parameters!(F)[0] _0);");
}

///
unittest {
    import std.format;
    import std.traits : FunctionAttribute;
    import bolts.experimental.refraction;

    interface GrandTour {
        pure int foo() immutable;
        @nogc @trusted nothrow ref int foo(out real, return ref int, lazy int) const;
        @safe shared scope void bar(scope Object);
    }

    class Mock(Interface) : Interface {
        static foreach (member; __traits(allMembers, Interface)) {
            static foreach (fun; __traits(getOverloads, Interface, member)) {
                mixin({
                        enum Model = refract!(fun, "fun");
                        if (is(ReturnType!fun == void)) {
                            return Model.withBody("{}").mixture;
                        } else if (Model.attributes & FunctionAttribute.ref_) {
                            return Model.withBody(q{{
                                        static %s rv;
                                        return rv;
                                    }}.format(Model.returnType)).mixture;
                        } else {
                            return Model.withBody(q{{
                                        return %s.init;
                                    }}.format(Model.returnType)).mixture;
                        }
                    }());
            }
        }
    }

    GrandTour mock = new Mock!GrandTour;
    real x;
    int i, l;
    mock.foo(x, i, l++) = 1;
    assert(mock.foo(x, i, l++) == 1);
    assert(l == 0);
}

private enum isFunctionContainer(alias T) = is(T == module) || is(T == struct)
    || is(T == class) || is(T == interface) || is(T == union);

/**
   Return a `Function` object refracting the function overload identified by
   `name` and `index` in `Scope`, using the value of `localName` to represent
   `Scope`. Store `index` in the `index` property of the Function.

   Params:
   Scope = a struct, class, interface, or union
   localName = a string mixin that represents `Scope`
   name = the name of the function
   index = the index of the function in the set of overloaded function called `name`
*/

Function refract(alias Scope, string localName, string name, uint index)()
if (isFunctionContainer!Scope) {
    return refract!(
        __traits(getOverloads, Scope, name)[index],
        `__traits(getOverloads, %s, "%s")[%d]`.format(localName, name, index))
        .withIndex(index);
}

///
unittest {
    interface Answers {
        int answer();
        string answer();
    }

    alias Container = Answers;

    enum answer0 = refract!(Container, "Container", "answer", 0);
    static assert(
        answer0.mixture ==
        q{@system bolts.experimental.refraction.ReturnType!(__traits(getOverloads, Container, "answer")[0]) answer();});
    static assert(answer0.index == 0);

    enum answer1 = refract!(Container, "Container", "answer", 1);
    static assert(
        answer1.mixture ==
        q{@system bolts.experimental.refraction.ReturnType!(__traits(getOverloads, Container, "answer")[1]) answer();});
    static assert(answer1.index == 1);
}

private enum True(T...) = true;

/**
   Return an array of `Function` objects, refracting the functions in `Scope`
   for which `IncludePredicate` evaluates to `true`, using the value of `localName` to
   represent `Scope`. `IncludePredicate` is optional; if not specified, refract all
   the functions. The `index` property of each `Function` is set to the index
   of the function in its *entire* overload set (i.e. including the overloads
   that may have been excluded by `IncludePredicate`).

   Applying this function to a module, without specifying `IncludePredicate`, may
   severely affect compilation time, as *all* the properties of *all* functions
   in the module will be queried.

   Params:
   Scope = an aggregate or a module
   localName = a string mixin that represents `Scope`
   IncludePredicate = a template that takes an alias to a function and evaluates to a compile time boolean
*/

auto refract(alias Scope, string localName, alias IncludePredicate = True)()
if (isFunctionContainer!Scope) {
    Function[] functions;

    static foreach (member; __traits(allMembers, Scope)) {
        static foreach (index, fun; __traits(getOverloads, Scope, member)) {
            static if (IncludePredicate!fun) {
                functions ~= refract!(Scope, localName, member, index);
            }
        }
    }

    return functions;
}

///
unittest {
    static union Answers {
        int answer();
        void answer();
        string answer();
    }

    alias Container = Answers;

    enum NotVoid(alias F) = !is(ReturnType!(F) == void);

    enum functions = refract!(Container, "Container", NotVoid);

    static assert(functions.length == 2);

    static assert(
        functions[0].mixture ==
        q{@system bolts.experimental.refraction.ReturnType!(__traits(getOverloads, Container, "answer")[0]) answer();});
    static assert(functions[0].index == 0);

    static assert(
        functions[1].mixture ==
        q{@system bolts.experimental.refraction.ReturnType!(__traits(getOverloads, Container, "answer")[2]) answer();});
    static assert(functions[1].index == 2);
}

private enum isAggregate(T...) =
    is(T[0] == struct) || is(T[0] == union) || is(T[0] == class)
    || is(T[0] == interface);

private mixin template replaceAttribute(string Name) {
    alias Struct = typeof(this);
    mixin(
        "Struct copy = {",
        {
            string[] mixture;
            foreach (member; __traits(allMembers, Struct)) {
                if (__traits(getOverloads, Struct, member).length == 0) {
                    mixture ~= member ~ ":" ~ (member == Name ? "value" : member);
                }
            }
            return mixture.join(",\n");
        }(),
        "};"
    );
}

unittest {
    static struct QA {
        string question;
        int answer;
        QA withQuestion(string value) {
            mixin replaceAttribute!"question";
            return copy;
        }
    }
    QA deflt = { answer: 42 };
    enum question = "How many roads must a man walk down?";
    auto result = deflt.withQuestion(question);
    assert(result.question == question);
    assert(result.answer == 42);
}

/**
   A struct capturing all the aspects of a function necessary to produce a
   string mixin that re-creates the function (excepting the body).
*/

immutable struct Function {

    /**
       A string that evaluates to a symbol representing the function in the
       local context.
    */

    string localName;

    /**
       Function name. Initial value: `__traits(identifier, fun)`.
    */

    string name;

    /**
       Return a new `Function` object with the `name` attribute set to `value`.
    */

    Function withName(string value) {
        mixin replaceAttribute!"name";
        return copy;
    }

    ///
    unittest {
        pure @nogc int answer();
        mixin(refract!(answer, "answer").withName("ultimateAnswer").mixture);
        static assert(
            __traits(getAttributes, ultimateAnswer) ==
            __traits(getAttributes, answer));
    }

    /**
       Index of function in an arbitrary collection of functions. The index can
       be set explicitly with `withIndex`. The `refract` overloads that operate
       on function containers set `index` to the position of the function in
       the set of overloaded functions in a scope. Default value: -1.
    */

    int index;

    /**
       Return a new `Function` object with the `index` attribute set to
       `value`.
    */

    Function withIndex(int value) {
        mixin replaceAttribute!"index";
        return copy;
    }

    /**
       Return type. Initial value: `bolts.experimental.refraction.ReturnType!fun`.
    */

    string returnType;

    /**
       Return a new `Function` object with the `returnType` attribute set to
       `value`.
    */

    Function withReturnType(string value) {
        mixin replaceAttribute!"returnType";
        return copy;
    }

    ///
    unittest {
        pure int answer() { return 42; }
        mixin(
            refract!(answer, "answer")
            .withName("realAnswer")
            .withReturnType("real")
            .mixture);
        static assert(is(typeof(realAnswer()) == real));
        static assert(functionAttributes!realAnswer & FunctionAttribute.pure_);
    }

    /**
       Function parameters. Initial value: from the refracted function.
    */

    Parameter[] parameters;

    /**
       Return a new `Function` object with the parameters attribute set to
       `value`.
    */

    Function withParameters(immutable(Parameter)[] value) {
        mixin replaceAttribute!"parameters";
        return copy;
    }

    ///
    unittest {
        int answer();
        mixin(
            refract!(answer, "answer")
            .withName("answerQuestion")
            .withParameters([ Parameter().withName("question").withType("string")])
            .mixture);
        int control(string);
        static assert(is(Parameters!answerQuestion == Parameters!control));
    }

    /**
       Return a new `Function` object with `newParameters` inserted at the
       specified `index` in the `attributes`.
    */

    Function insertParameters(uint index, immutable(Parameter)[] newParameters...) {
        auto value = index == parameters.length ? parameters ~ newParameters
            : index == 0 ? newParameters ~ parameters
            : parameters[0..index] ~ newParameters ~ parameters[index..$];
        mixin replaceAttribute!"parameters";
        return copy;
    }

    /**
       Function body. Initial value: `;`.
    */

    string body_;

    /**
       Return a new `Function` object with the `body_` attribute set to
       `value`.
    */

    Function withBody(string value) {
        mixin replaceAttribute!"body_";
        return copy;
    }

    ///
    unittest {
        pure int answer();
        mixin(
            refract!(answer, "answer").withName("theAnswer")
            .withBody("{ return 42; }")
            .mixture);
        static assert(theAnswer() == 42);
    }

    /**
       Function attributes.
       Initial value: `std.traits.functionAttributes!fun`
    */

    ulong attributes;

    /**
       Return a new `Function` object with the `attributes` attribute set to
       `value`.
    */

    Function withAttributes(uint value) {
        mixin replaceAttribute!"attributes";
        return copy;
    }

    ///
    unittest {
        nothrow int answer();
        enum model = refract!(answer, "answer");
        with (FunctionAttribute) {
            mixin(
                model
                .withName("pureAnswer")
                .withAttributes(model.attributes | pure_)
                .mixture);
            static assert(functionAttributes!pureAnswer & pure_);
            static assert(functionAttributes!pureAnswer & nothrow_);
        }
    }

    /**
       If `true`, prefix generated function with `static`. Initial value:
       `true` if the refracted function is a static *member* function inside a
       struct, class, interface, or union.
    */

    bool static_;

    /**
       Return a new `Function` object with the `static_` attribute set to
       `value`.
    */

    Function withStatic(bool value) {
        mixin replaceAttribute!"static_";
        return copy;
    }

    ///
    unittest {
        struct Question {
            static int answer() { return 42; }
        }
        mixin(
            refract!(Question.answer, "Question.answer")
            .withStatic(false)
            .withBody("{ return Question.answer; }")
            .mixture);
        static assert(answer() == 42);
    }

    /**
       User defined attributes.
       Initial value:
       `bolts.experimental.refraction.ParameterAttribute!(fun, parameterIndex..., attributeIndex...)`.
    */

    string[] udas;

    /**
       Return a new `Function` object with the `udas` attribute set to `value`.
    */

    Function withUdas(immutable(string)[] value) {
        mixin replaceAttribute!"udas";
        return copy;
    }

    ///
    unittest {
        import std.typecons : tuple;
        @(666) int answer();

        mixin(
            refract!(answer, "answer")
            .withName("answerIs42")
            .withUdas(["@(42)"])
            .mixture);
        static assert(__traits(getAttributes, answerIs42).length == 1);
        static assert(__traits(getAttributes, answerIs42)[0] == 42);
    }

    /**
       Return mixin code for this `Function`.
    */

    string mixture() {
        return join(
            udas ~
            attributeMixtureArray() ~
            [
                returnType,
                name ~ "(" ~ parameterListMixtureArray.join(", ") ~ ")",
            ], " ") ~
            body_;
    }

    string[] parameterListMixtureArray() {
        return map!(p => p.mixture)(parameters).array;
    }

    /**
       Return the argument list as an array of strings.
    */

    const(string)[] argumentMixtureArray() {
        return parameters.map!(p => p.name).array;
    }

    ///
    unittest {
        int add(int a, int b);
        static assert(refract!(add, "add").argumentMixtureArray == [ "_0", "_1" ]);
    }

    /**
       Return the argument list as a string.
    */

    string argumentMixture() {
        return argumentMixtureArray.join(", ");
    }

    ///
    unittest {
        int add(int a, int b);
        static assert(refract!(add, "add").argumentMixture == "_0, _1");
    }

    /**
       Return the attribute list as an array of strings.
    */

    string[] attributeMixtureArray() {
        with (FunctionAttribute) {
            return []
                ~ (static_ ? ["static"] : [])
                ~ (attributes & pure_ ? ["pure"] : [])
                ~ (attributes & nothrow_ ? ["nothrow"] : [])
                ~ (attributes & property ? ["@property"] : [])
                ~ (attributes & trusted ? ["@trusted"] : [])
                ~ (attributes & safe ? ["@safe"] : [])
                ~ (attributes & nogc ? ["@nogc"] : [])
                ~ (attributes & system ? ["@system"] : [])
                ~ (attributes & const_ ? ["const"] : [])
                ~ (attributes & immutable_ ? ["immutable"] : [])
                ~ (attributes & inout_ ? ["inout"] : [])
                ~ (attributes & shared_ ? ["shared"] : [])
                ~ (attributes & return_ ? ["return"] : [])
                ~ (attributes & scope_ ? ["scope"] : [])
                ~ (attributes & ref_ ? ["ref"] : [])
                ;
        }
    }

    ///
    unittest {
        nothrow pure int answer();
        enum model = refract!(answer, "answer");
        static assert(
            model.attributeMixtureArray == ["pure", "nothrow", "@system"]);
    }

    /**
       Return the attribute list as a string.
    */

    string attributeMixture() {
        return attributeMixtureArray.join(" ");
    }

    ///
    unittest {
        nothrow pure int answer();
        enum model = refract!(answer, "answer");
        static assert(model.attributeMixture == "pure nothrow @system");
    }
}

/**
   A struct capturing all the properties of a function parameter.
*/

immutable struct Parameter {
    /**
       Parameter name. Initial value: `_i`, where `i` is the position of the
       parameter.
    */

    string name;

    /**
       Return a new Parameter object with the `name` attribute set to `value`.
    */

    Parameter withName(string value) {
        mixin replaceAttribute!"name";
        return copy;
    }

    /**
       Parameter type. Initial value: `std.traits.Parameter!fun[i]`, where
       `fun` is the refracted function and `i` is the position of the
       parameter.
    */

    string type;

    /**
       Return a new `Parameter` object with the `type` attribute set to
       `value`.
    */

    Parameter withType(string value) {
        mixin replaceAttribute!"type";
        return copy;
    }

    /**
       Parameter storage classes. Initial value:
       `[__traits(getParameterStorageClasses, fun, i)]`, where where `fun` is
       the refracted function and `i` is the position of the parameter.
    */

    string[] storageClasses;

    /**
       Return a new `Parameter` object with the `storageClasses` attribute set
       to `value`.
    */

    Parameter withStorageClasses(immutable(string)[] value) {
        mixin replaceAttribute!"storageClasses";
        return copy;
    }

    /**
       Parameter UDAs. Initial value:
       `[@(bolts.experimental.refraction.ParameterAttribute!(fun,i, j...))]`,
       where where `fun` is the refracted function, `i` is the position of the
       parameter, and `j...` are the positions of the UDAs.
    */

    string[] udas;

    /**
       Return a new `Parameter` object with the `udas` attribute set to
       `value`.
    */

    Parameter withUdas(immutable(string)[] value) {
        mixin replaceAttribute!"udas";
        return copy;
    }

    string mixture() {
        return join(udas ~ storageClasses ~ [ type, name ], " ");
    }
}

private Parameter refractParameter(alias Fun, string mixture, uint index)() {
    static if (is(typeof(Fun) parameters == __parameters)) {
        alias parameter = parameters[index .. index + 1];
        static if (__traits(compiles,  __traits(getAttributes, parameter))) {
            enum udaFormat = "@(bolts.experimental.refraction.ParameterAttribute!(%s, %d, %%d))".format(
                mixture, index);
            enum udas = __traits(getAttributes, parameter).length.iota.map!(
                formatIndex!udaFormat).array;
        } else {
            enum udas = [];
        }

        Parameter p = {
            type: `bolts.experimental.refraction.Parameters!(%s)[%d]`.format(mixture, index),
            name: "_%d".format(index),
            storageClasses: [__traits(getParameterStorageClasses, Fun, index)],
            udas: udas,
        };
    }
    return p;
}

private Parameter[] refractParameterList(alias Fun, string mixture)() {
    Parameter[] result;
    static if (is(typeof(Fun) parameters == __parameters)) {
        static foreach (i; 0 .. parameters.length) {
            result ~= refractParameter!(Fun, mixture, i);
        }
    }
    return result;
}

private string formatIndex(string f)(ulong i) {
    return format!f(i);
}
