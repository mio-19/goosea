package goosea.truffle;

import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.frame.VirtualFrame;

@NodeInfo(language = "Goosea", description = "The abstract node")
public abstract class GooseaAbstractNode extends Node {
    public abstract void execute(VirtualFrame frame);
}
