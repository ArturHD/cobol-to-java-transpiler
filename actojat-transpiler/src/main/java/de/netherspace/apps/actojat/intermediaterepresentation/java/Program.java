package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.Getter;

import java.util.LinkedList;
import java.util.List;

public class Program extends JavaLanguageConstruct {

  @Getter
  private List<Method> methods;

  @Getter
  private List<Import> imports;

  /**
   * Default constructor.
   */
  public Program() {
    super();
    this.methods = new LinkedList<>();
    this.imports = new LinkedList<>();
  }

}
