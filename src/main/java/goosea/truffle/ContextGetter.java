package goosea.truffle;

import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import org.jetbrains.annotations.NotNull;


class ContextGetter extends RootNode {
    public ContextGetter(@NotNull GooseaLang lang) {
        super(lang, null);
    }

    @Override
    public Context execute(@NotNull VirtualFrame frame) {
        return Context.get(this);
    }

    public @NotNull DirectCallNode createDirectCallNode() {
        return Truffle.getRuntime().createDirectCallNode(this.getCallTarget());
    }

    public static Context get(@NotNull GooseaLang lang) {
        return (Context) new ContextGetter(lang).createDirectCallNode().call();
    }
}
