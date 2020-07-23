module bolts.experimental.problem;

import bolts.experimental.mock;

struct Problem {}

interface GrandTour {
    pure Problem foo() immutable; // Ha!
    @nogc @trusted nothrow ref int foo(out real, return ref int, lazy int) const;
    @safe shared scope void bar(scope Object);
}

unittest {
    auto gt = new Mock!GrandTour();
}
