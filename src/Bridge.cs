using System;
using System.IO;
using System.Text;
using System.Globalization;
using ZwSoft.ZwCAD.Runtime;
using ZwSoft.ZwCAD.DatabaseServices;
using A = ZwSoft.ZwCAD.ApplicationServices.Application;
[assembly: CommandClass(typeof(ZWKitBridge102))]
public class ZWKitBridge102 {
    static string FileName { get { return Path.Combine(Path.GetTempPath(), "zwkit-bridge-102.txt"); } }
    static void Write(string value) { File.WriteAllText(FileName, value, Encoding.ASCII); }
    [CommandMethod("ZWKREADY102")]
    public static void Ready() { Write("(\"READY\")"); }
    [CommandMethod("ZWKSMOOTH102")]
    public static void Smooth() {
        var db=A.DocumentManager.MdiActiveDocument.Database;
        using(var tr=db.TransactionManager.StartTransaction()) {
            var vp=(ViewportTableRecord)tr.GetObject(db.CurrentViewportTableRecordId,OpenMode.ForRead);
            Write(vp.CircleSides.ToString(CultureInfo.InvariantCulture));
        }
    }
    static void Emit(ResultBuffer result) {
        var s=new StringBuilder("(");
        foreach(var v in result.AsArray()) {
            if(v.TypeCode==(int)LispDataType.ListBegin) s.Append("(");
            else if(v.TypeCode==(int)LispDataType.ListEnd) s.Append(") ");
            else if(v.TypeCode==(int)LispDataType.Text) s.Append("\"").Append(v.Value).Append("\" ");
            else s.Append(Convert.ToString(v.Value,CultureInfo.InvariantCulture)).Append(" ");
        }
        s.Append(")"); Write(s.ToString());
    }
    static void CaptureCommon(bool keys) {
        Write("(\"ERROR\")");
        var sz=(ZwSoft.ZwCAD.Geometry.Point2d)A.GetSystemVariable("SCREENSIZE");
        using(var args=new ResultBuffer(new TypedValue((int)LispDataType.Int32,(int)sz.X),new TypedValue((int)LispDataType.Int32,(int)sz.Y)))
        using(var result= keys ? ZWKitMouse.CaptureKeys(args) : ZWKitMouse.Capture(args)) {
            Emit(result);
        }
    }
    // 拖动修剪 / 拖动延伸用（既有行为，不要改）。
    [CommandMethod("ZWKCAP102")]
    public static void Capture() { CaptureCommon(false); }
    // TR / EX 交互层用：多一个 ("KEY" "C") 返回形态（用户按下的字母/数字键）。
    [CommandMethod("ZWKCAP103")]
    public static void CaptureKeys() { CaptureCommon(true); }
}
