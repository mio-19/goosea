package goosea.truffle;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.joou.ULong;
import org.jetbrains.annotations.NotNull;

@NodeInfo(language = "Goosea", description = "The base node")
public abstract class GooseaNode extends Node {
    public abstract void execute(@NotNull VirtualFrame frame);

    public abstract ULong pc();
}
