/*******************************************************************************
 * Copyright (c) 2012 Artem Melentyev <amelentev@gmail.com>.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * GNU Public License v2.0 + OpenJDK assembly exception.
 * 
 * Contributors:
 *     Artem Melentyev <amelentev@gmail.com> - initial API and implementation
 ******************************************************************************/
package javaoo.javac8;

import com.sun.source.util.TaskEvent;
import com.sun.source.util.TaskListener;
import com.sun.tools.javac.api.MultiTaskListener;
import com.sun.tools.javac.comp.*;
import com.sun.tools.javac.main.JavaCompiler;
import com.sun.tools.javac.processing.JavacProcessingEnvironment;
import com.sun.tools.javac.util.Context;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Set;

@SupportedAnnotationTypes("*")
@SupportedSourceVersion(SourceVersion.RELEASE_8)
public class OOProcessor extends AbstractProcessor {
    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);
        JavacProcessingEnvironment pe = (JavacProcessingEnvironment) processingEnv;
        JavaCompiler compiler = JavaCompiler.instance(pe.getContext());
        try {
            processingEnv.getMessager().printMessage(Diagnostic.Kind.NOTE, "Injecting OO to javac8");
            MultiTaskListener taskListener = (MultiTaskListener) get(compiler, "taskListener");
            taskListener.add(new WaitAnalyzeTaskListener(compiler));
        } catch (Exception e) {
            sneakyThrow(e);
        }
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        return false;
    }

    private static final class WaitAnalyzeTaskListener implements TaskListener {
        final JavaCompiler compiler;
        boolean done = false;
        WaitAnalyzeTaskListener(JavaCompiler compiler) {
            this.compiler = compiler;
        }
        @Override
        public void started(TaskEvent e) {
            if (!done && e.getKind() == TaskEvent.Kind.ANALYZE) {
                patch(compiler);
                done = true;
            }
        }
        @Override
        public void finished(TaskEvent e) {}
    }

    private static void patch(JavaCompiler compiler) {
        try {
            JavaCompiler delCompiler = (JavaCompiler) get(compiler, "delegateCompiler");
            if (delCompiler != null)
                compiler = delCompiler; // javac has delegateCompiler. netbeans hasn't
            Context context = (Context) get(compiler, "context");
            Attr attr = Attr.instance(context);
            if (attr instanceof OOAttr)
                return;
            ClassLoader destcl = attr.getClass().getClassLoader();

            // hack: load classes to the same classloader so they will be able to use and override default accessor members
            Class<?> attrClass = reloadClass(OOAttr.class.getName(), destcl);
            Class<?> resolveClass = reloadClass(OOResolve.class.getName(), destcl);
            Class<?> transTypesClass = reloadClass(OOTransTypes.class.getName(), destcl);
            reloadClass(javaoo.OOMethods.class.getName(), destcl);
            reloadClass("javaoo.OOMethods$1", destcl);
            reloadClass("javaoo.OOMethods$2", destcl);

            getInstance(resolveClass, context);
            attr = (Attr) getInstance(attrClass, context);
            Object transTypes = getInstance(transTypesClass, context);

            set(compiler, JavaCompiler.class, "attr", attr);
            set(compiler, JavaCompiler.class, "transTypes", transTypes);
            set(MemberEnter.instance(context), MemberEnter.class, "attr", attr);
        } catch (Exception e) {
            sneakyThrow(e);
        }
    }

    /** add class claz to outClassLoader */
    private static Class<?> reloadClass(final String claz, ClassLoader outcl) throws Exception {
        try { // already loaded?
            return outcl.loadClass(claz);
        } catch (ClassNotFoundException e) {}
        String path = claz.replace('.', '/') + ".class";
        InputStream is = OOProcessor.class.getClassLoader().getResourceAsStream(path);
        byte[] bytes = new byte[is.available()];
        is.read(bytes);
        Method m = ClassLoader.class.getDeclaredMethod("defineClass", new Class[] {
                String.class, byte[].class, int.class, int.class });
        m.setAccessible(true);
        return (Class<?>) m.invoke(outcl, claz, bytes, 0, bytes.length);
    }

    // reflection stuff
    private static Object getInstance(Class<?> clas, Context context) throws ReflectiveOperationException {
        return clas.getDeclaredMethod("instance", Context.class).invoke(null, context);
    }

    private static Object get(Object obj, String field) throws ReflectiveOperationException {
        Field f = JavaCompiler.class.getDeclaredField(field);
        f.setAccessible(true);
        return f.get(obj);
    }
    private static void set(Object obj, Class clas, String field, Object val) throws ReflectiveOperationException {
        Field f = clas.getDeclaredField(field);
        f.setAccessible(true);
        f.set(obj, val);
    }
    private static void sneakyThrow(Throwable ex) {
        OOProcessor.<RuntimeException>sneakyThrowInner(ex);
    }
    private static <T extends Throwable> T sneakyThrowInner(Throwable ex) throws T {
        throw (T) ex;
    }
}
