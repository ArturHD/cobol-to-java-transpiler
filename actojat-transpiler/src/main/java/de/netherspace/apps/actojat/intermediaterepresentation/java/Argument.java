package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class Argument extends JavaLanguageConstruct {

  private String type;
  private String name;

}
