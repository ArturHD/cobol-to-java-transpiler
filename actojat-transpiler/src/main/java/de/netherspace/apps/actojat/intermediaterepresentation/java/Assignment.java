package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class Assignment extends Statement {

  private String lhs;
  private String rhs;

}
