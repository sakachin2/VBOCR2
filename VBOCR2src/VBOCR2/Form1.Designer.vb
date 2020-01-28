<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form1
    Inherits System.Windows.Forms.Form
    'フォームがコンポーネントの一覧をクリーンアップするために dispose をオーバーライドします。
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Windows フォーム デザイナーで必要です。
    Private components As System.ComponentModel.IContainer

    'メモ: 以下のプロシージャは Windows フォーム デザイナーで必要です。
    'Windows フォーム デザイナーを使用して変更できます。  
    'コード エディターを使って変更しないでください。
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form1))
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.FileToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ImageToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.KanjiTextToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExitXToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemOptions = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemHelp = New System.Windows.Forms.ToolStripMenuItem()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog()
        Me.FontDialog1 = New System.Windows.Forms.FontDialog()
        Me.NotifyIcon1 = New System.Windows.Forms.NotifyIcon(Me.components)
        Me.ContextMenuSpecialChar = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.CMCut = New System.Windows.Forms.ToolStripMenuItem()
        Me.CMCopy = New System.Windows.Forms.ToolStripMenuItem()
        Me.CMPaste = New System.Windows.Forms.ToolStripMenuItem()
        Me.CMSelectAll = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemSpecialChar = New System.Windows.Forms.ToolStripMenuItem()
        Me.CMFind = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemWords = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemLetterReplacement = New System.Windows.Forms.ToolStripMenuItem()
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.ToolStripStatusLabel1 = New System.Windows.Forms.ToolStripStatusLabel()
        Me.TBBES = New System.Windows.Forms.TextBox()
        Me.MenuStrip1.SuspendLayout()
        Me.ContextMenuSpecialChar.SuspendLayout()
        Me.StatusStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'MenuStrip1
        '
        resources.ApplyResources(Me.MenuStrip1, "MenuStrip1")
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FileToolStripMenuItem, Me.ToolStripMenuItemOptions, Me.ToolStripMenuItemHelp})
        Me.MenuStrip1.Name = "MenuStrip1"
        '
        'FileToolStripMenuItem
        '
        resources.ApplyResources(Me.FileToolStripMenuItem, "FileToolStripMenuItem")
        Me.FileToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.FileToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ImageToolStripMenuItem, Me.KanjiTextToolStripMenuItem, Me.ExitXToolStripMenuItem})
        Me.FileToolStripMenuItem.Name = "FileToolStripMenuItem"
        '
        'ImageToolStripMenuItem
        '
        resources.ApplyResources(Me.ImageToolStripMenuItem, "ImageToolStripMenuItem")
        Me.ImageToolStripMenuItem.Name = "ImageToolStripMenuItem"
        '
        'KanjiTextToolStripMenuItem
        '
        resources.ApplyResources(Me.KanjiTextToolStripMenuItem, "KanjiTextToolStripMenuItem")
        Me.KanjiTextToolStripMenuItem.Name = "KanjiTextToolStripMenuItem"
        '
        'ExitXToolStripMenuItem
        '
        resources.ApplyResources(Me.ExitXToolStripMenuItem, "ExitXToolStripMenuItem")
        Me.ExitXToolStripMenuItem.Name = "ExitXToolStripMenuItem"
        '
        'ToolStripMenuItemOptions
        '
        resources.ApplyResources(Me.ToolStripMenuItemOptions, "ToolStripMenuItemOptions")
        Me.ToolStripMenuItemOptions.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripMenuItemOptions.Name = "ToolStripMenuItemOptions"
        '
        'ToolStripMenuItemHelp
        '
        resources.ApplyResources(Me.ToolStripMenuItemHelp, "ToolStripMenuItemHelp")
        Me.ToolStripMenuItemHelp.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripMenuItemHelp.Name = "ToolStripMenuItemHelp"
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.FileName = "OpenFileDialog1"
        resources.ApplyResources(Me.OpenFileDialog1, "OpenFileDialog1")
        '
        'SaveFileDialog1
        '
        resources.ApplyResources(Me.SaveFileDialog1, "SaveFileDialog1")
        '
        'NotifyIcon1
        '
        resources.ApplyResources(Me.NotifyIcon1, "NotifyIcon1")
        '
        'ContextMenuSpecialChar
        '
        resources.ApplyResources(Me.ContextMenuSpecialChar, "ContextMenuSpecialChar")
        Me.ContextMenuSpecialChar.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CMCut, Me.CMCopy, Me.CMPaste, Me.CMSelectAll, Me.ToolStripMenuItemSpecialChar, Me.CMFind, Me.ToolStripMenuItemWords, Me.ToolStripMenuItemLetterReplacement})
        Me.ContextMenuSpecialChar.Name = "ContextMenuStrip1"
        '
        'CMCut
        '
        resources.ApplyResources(Me.CMCut, "CMCut")
        Me.CMCut.Name = "CMCut"
        '
        'CMCopy
        '
        resources.ApplyResources(Me.CMCopy, "CMCopy")
        Me.CMCopy.Name = "CMCopy"
        '
        'CMPaste
        '
        resources.ApplyResources(Me.CMPaste, "CMPaste")
        Me.CMPaste.Name = "CMPaste"
        '
        'CMSelectAll
        '
        resources.ApplyResources(Me.CMSelectAll, "CMSelectAll")
        Me.CMSelectAll.Name = "CMSelectAll"
        '
        'ToolStripMenuItemSpecialChar
        '
        resources.ApplyResources(Me.ToolStripMenuItemSpecialChar, "ToolStripMenuItemSpecialChar")
        Me.ToolStripMenuItemSpecialChar.Name = "ToolStripMenuItemSpecialChar"
        '
        'CMFind
        '
        resources.ApplyResources(Me.CMFind, "CMFind")
        Me.CMFind.Name = "CMFind"
        '
        'ToolStripMenuItemWords
        '
        resources.ApplyResources(Me.ToolStripMenuItemWords, "ToolStripMenuItemWords")
        Me.ToolStripMenuItemWords.Name = "ToolStripMenuItemWords"
        '
        'ToolStripMenuItemLetterReplacement
        '
        resources.ApplyResources(Me.ToolStripMenuItemLetterReplacement, "ToolStripMenuItemLetterReplacement")
        Me.ToolStripMenuItemLetterReplacement.Name = "ToolStripMenuItemLetterReplacement"
        '
        'StatusStrip1
        '
        resources.ApplyResources(Me.StatusStrip1, "StatusStrip1")
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripStatusLabel1})
        Me.StatusStrip1.Name = "StatusStrip1"
        '
        'ToolStripStatusLabel1
        '
        resources.ApplyResources(Me.ToolStripStatusLabel1, "ToolStripStatusLabel1")
        Me.ToolStripStatusLabel1.Name = "ToolStripStatusLabel1"
        '
        'TBBES
        '
        resources.ApplyResources(Me.TBBES, "TBBES")
        Me.TBBES.BackColor = System.Drawing.SystemColors.Info
        Me.TBBES.ContextMenuStrip = Me.ContextMenuSpecialChar
        Me.TBBES.Cursor = System.Windows.Forms.Cursors.Default
        Me.TBBES.HideSelection = False
        Me.TBBES.Name = "TBBES"
        Me.TBBES.ShortcutsEnabled = False
        '
        'Form1
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.TBBES)
        Me.Controls.Add(Me.StatusStrip1)
        Me.Controls.Add(Me.MenuStrip1)
        Me.MainMenuStrip = Me.MenuStrip1
        Me.Name = "Form1"
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.ContextMenuSpecialChar.ResumeLayout(False)
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents MenuStrip1 As System.Windows.Forms.MenuStrip
    Friend WithEvents FileToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents OpenFileDialog1 As System.Windows.Forms.OpenFileDialog
    Friend WithEvents SaveFileDialog1 As System.Windows.Forms.SaveFileDialog
    Friend WithEvents MRUImage As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents MRUText As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents MRUKanaText As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents MRUBESText As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ExitXToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ImageToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents KanjiTextToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FontDialog1 As System.Windows.Forms.FontDialog
    Friend WithEvents ToolStripMenuItemOptions As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NotifyIcon1 As System.Windows.Forms.NotifyIcon
    Friend WithEvents ToolStripMenuItemHelp As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ContextMenuSpecialChar As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents CMCut As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CMCopy As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CMPaste As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CMSelectAll As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemSpecialChar As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CMFind As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents StatusStrip1 As System.Windows.Forms.StatusStrip
    Friend WithEvents ToolStripStatusLabel1 As System.Windows.Forms.ToolStripStatusLabel
    Friend WithEvents ToolStripMenuItemWords As ToolStripMenuItem
    Friend WithEvents TBBES As TextBox
    Friend WithEvents ToolStripMenuItemLetterReplacement As ToolStripMenuItem
    '   Friend WithEvents ContextMenuStrip1 As System.Windows.Forms.ContextMenuStrip
End Class
