package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@AllArgsConstructor
public class Expression extends Statement {

  @Getter
  @Setter
  private String[] parts;

}
