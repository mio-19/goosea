package goosea.truffle;

import com.oracle.truffle.api.TruffleLanguage;
import org.jetbrains.annotations.NotNull;

@TruffleLanguage.Registration(id = "goosea", name = "Goosea", characterMimeTypes = GooseaLang.MIME_TYPE, defaultMimeType = GooseaLang.MIME_TYPE)
public final class GooseaLang extends TruffleLanguage<Context>{
    public static final String MIME_TYPE = "application/x-goosea";

    @Override
    protected @NotNull Context createContext(Env env) {
        return Context.apply(this);
    }

    public @NotNull Context getContext() {
        return ContextGetter.get(this);
    }

    static {
        // check if GraalVM runtime is installed
        try {
            Class.forName("com.oracle.truffle.api.impl.DefaultTruffleRuntime");
            if (!System.getProperty("java.vm.vendor").contains("GraalVM")) {
                throw new ClassNotFoundException("GraalVM runtime is not installed");
            }
        } catch (ClassNotFoundException e) {
            throw new RuntimeException("Goosea requires GraalVM to be installed.");
        }
    }
}
