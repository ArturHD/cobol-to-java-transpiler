package de.netherspace.apps.actojat.intermediaterepresentation.java;

public class Assignment extends Statement {

	private String lhs;
	private String rhs;
	
	public String getLhs() {
		return lhs;
	}
	
	public void setLhs(String lhs) {
		this.lhs = lhs;
	}
	
	public String getRhs() {
		return rhs;
	}
	
	public void setRhs(String rhs) {
		this.rhs = rhs;
	}
	
}
