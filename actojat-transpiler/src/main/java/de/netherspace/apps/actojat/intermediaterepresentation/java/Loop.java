package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
abstract class Loop extends Statement {

  private Statement[] body;

}
