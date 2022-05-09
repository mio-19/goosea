package goosea.truffle;

import com.oracle.truffle.api.TruffleLanguage;
import org.jetbrains.annotations.NotNull;

@TruffleLanguage.Registration(id = "goosea", name = "Goosea", characterMimeTypes = GooseaLang.MIME_TYPE, defaultMimeType = GooseaLang.MIME_TYPE)
public final class GooseaLang extends TruffleLanguage<Context>{
    public static final String MIME_TYPE = "application/x-goosea";

    @Override
    protected @NotNull Context createContext(Env env) {
        return Context.apply();
    }
}
