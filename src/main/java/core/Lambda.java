package core;

import java.util.List;

public class Lambda implements Term {
    public Lambda(List<Parameter> parameters, Term body) {
        this.body = body;
        this.parameters = parameters;
    }

    static public class Parameter {
        public Parameter(String name, Term type) {
            this.name = name;
            this.type = type;
        }

        public String name;
        public Term type;
    }

    public List<Parameter> parameters;
    public Term body;
}

