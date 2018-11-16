package de.netherspace.apps.actojat.intermediaterepresentation.java;

import java.util.LinkedList;
import java.util.List;

public class FunctionCall extends Statement {

    private String name;
    private List<String> parameters;

    public FunctionCall() {
        super();
        this.parameters = new LinkedList<>();
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<String> getParameters() {
        return parameters;
    }

}
