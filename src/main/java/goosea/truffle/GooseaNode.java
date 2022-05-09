package goosea.truffle;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.jetbrains.annotations.NotNull;
import goosea.utils.num.*;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;

@NodeInfo(language = "Goosea", description = "The node")
public class GooseaNode extends GooseaAbstractNode {
    // unsigned
    public final long pc;
    // unsigned
    @CompilationFinal
    private int instr;

    public final U64 getPC() {
        return U64.apply(this.pc);
    }

    private final U32 getInstr() {
        return U32.apply(this.instr);
    }

    public GooseaNode(long pc) {
        this.pc = pc;
    }

    public GooseaNode(U64 pc) {
        this.pc = pc.toLong();
    }


    @Override
    public void execute(VirtualFrame frame) {
        Context context = getContext();
        int currentInstr = context.cpu().fetchForMock(this.getPC()).toInt();
        if (currentInstr != this.instr) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var lock = getLock();
            lock.lock();
            this.instr = currentInstr;
            lock.unlock();
        }
        context.cpu().mockTick(this.getPC(), this.getInstr());
    }

    public final Context getContext() {
        return Context.get(this);
    }
}
