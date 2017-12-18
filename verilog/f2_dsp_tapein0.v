`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif

module f2_dsp_tapein0( // @[:@3.2]
  input         clock, // @[:@4.4]
  input         reset, // @[:@5.4]
  input         io_clock_DSP, // @[:@6.4]
  input  [15:0] io_iptr_Areal, // @[:@6.4]
  input  [15:0] io_iptr_Aimag, // @[:@6.4]
  output [15:0] io_Zreal, // @[:@6.4]
  output [15:0] io_Zimag // @[:@6.4]
);
  reg [15:0] inregreal; // @[f2_dsp_tapein0.scala 19:27:@8.4]
  reg [31:0] _RAND_0;
  reg [15:0] inregimag; // @[f2_dsp_tapein0.scala 20:27:@10.4]
  reg [31:0] _RAND_1;
  reg [15:0] _T_10; // @[f2_dsp_tapein0.scala 23:24:@12.4]
  reg [31:0] _RAND_2;
  reg [15:0] _T_12; // @[f2_dsp_tapein0.scala 24:24:@15.4]
  reg [31:0] _RAND_3;
  assign io_Zreal = _T_10;
  assign io_Zimag = _T_12;
`ifdef RANDOMIZE
  integer initvar;
  initial begin
    `ifndef verilator
      #0.002 begin end
    `endif
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{$random}};
  inregreal = _RAND_0[15:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{$random}};
  inregimag = _RAND_1[15:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{$random}};
  _T_10 = _RAND_2[15:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{$random}};
  _T_12 = _RAND_3[15:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    inregreal <= io_iptr_Areal;
    inregimag <= io_iptr_Aimag;
  end
  always @(posedge io_clock_DSP) begin
    _T_10 <= inregreal;
    _T_12 <= inregimag;
  end
endmodule
