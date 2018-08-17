package de.netherspace.apps.actojat.intermediaterepresentation.java;

import java.util.LinkedList;
import java.util.List;

public class Method extends JavaLanguageConstruct {

	private String name;
	private List<Statement> statements;
	private List<Argument> arguments;

	public Method() {
		super();
		this.statements = new LinkedList<>();
		this.arguments = new LinkedList<>();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<Statement> getStatements() {
		return statements;
	}

	public List<Argument> getArguments() {
		return arguments;
	}

}
