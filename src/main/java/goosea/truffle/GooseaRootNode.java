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

public class GooseaRootNode extends RootNode {
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private GooseaAbstractNode node;

    public GooseaRootNode(@NotNull GooseaLang lang, @NotNull GooseaAbstractNode node) {
        super(lang, null);
        this.node = node;
    }

    @Override
    public Object execute(@NotNull VirtualFrame frame) {
        node.execute(frame);
        return null;
    }

    public @NotNull DirectCallNode createDirectCallNode() {
        return Truffle.getRuntime().createDirectCallNode(this.getCallTarget());
    }
}
