package goosea.truffle;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class GooseaRootNode extends RootNode {
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private GooseaAbstractNode node;

    public GooseaRootNode(GooseaAbstractNode node) {
        super(null, null);
        this.node = node;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        node.executeGeneric(frame);
        return null;
    }
}
