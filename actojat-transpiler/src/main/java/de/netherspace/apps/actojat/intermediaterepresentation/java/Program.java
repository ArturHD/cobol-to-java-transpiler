package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.Getter;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class Program extends JavaLanguageConstruct {

  @Getter
  private Map<String, Method> methods;

  @Getter
  private List<Import> imports;

  /**
   * Default constructor.
   */
  public Program() {
    super();
    this.methods = new HashMap<>();
    this.imports = new LinkedList<>();
  }

}
