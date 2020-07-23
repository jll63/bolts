module bolts.experimental.mock;

import bolts.experimental.refraction;
import std.format;
import std.traits;

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
