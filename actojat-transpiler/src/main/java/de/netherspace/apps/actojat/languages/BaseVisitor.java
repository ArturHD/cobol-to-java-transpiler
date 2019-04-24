package de.netherspace.apps.actojat.languages;

import de.netherspace.apps.actojat.ir.java.ArgumentDeclaration;

import java.util.Map;
import java.util.function.Function;

public interface BaseVisitor {

  /**
   * Maps a commonly found argument declaration (i.e. a tuple consisting of a type and a
   * name) to a Java argument.
   */
  Function<Map.Entry<String, String>, ArgumentDeclaration> argumentTupleToJavaArgument = e -> {
    final String name = e.getKey();
    final String type = e.getValue();
    ArgumentDeclaration jargument = new ArgumentDeclaration(type, name, null);
    return jargument;
  };

}
