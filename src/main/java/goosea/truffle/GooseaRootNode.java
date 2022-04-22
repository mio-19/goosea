package goosea.truffle;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class GooseaRootNode extends RootNode {
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private GooseaNode node;

    public GooseaRootNode(GooseaNode node) {
        super(null, null);
        this.node = node;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        node.execute(frame);
        return null;
    }
}
