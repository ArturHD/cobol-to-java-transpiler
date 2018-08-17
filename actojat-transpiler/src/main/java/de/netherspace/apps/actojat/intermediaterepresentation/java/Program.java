package de.netherspace.apps.actojat.intermediaterepresentation.java;

import java.util.LinkedList;
import java.util.List;

public class Program extends JavaLanguageConstruct {

	private List<Method> methods;
	private List<Import> imports;
	
	public Program() {
		super();
		this.methods = new LinkedList<>();
		this.imports = new LinkedList<>();
	}

	public List<Method> getMethods() {
		return methods;
	}

	public List<Import> getImports() {
		return imports;
	}

}
