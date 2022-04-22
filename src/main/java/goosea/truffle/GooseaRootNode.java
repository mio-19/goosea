package goosea.truffle;

import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;

public class GooseaRootNode extends RootNode {
    @Children
    @SuppressWarnings("FieldMayBeFinal")
    private GooseaNode[] nodes;

    public GooseaRootNode(GooseaNode[] nodes) {
        super(null, null);
        this.nodes = nodes;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        for (GooseaNode node : nodes) {
            node.execute(frame);
        }
        return null;
    }
}
