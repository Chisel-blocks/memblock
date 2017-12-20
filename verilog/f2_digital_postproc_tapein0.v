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

module f2_digital_postproc_tapein0( // @[:@3.2]
  input         clock, // @[:@4.4]
  input         reset, // @[:@5.4]
  input  [15:0] io_iptr_A_0_real, // @[:@6.4]
  input  [15:0] io_iptr_A_0_imag, // @[:@6.4]
  input  [15:0] io_iptr_A_1_real, // @[:@6.4]
  input  [15:0] io_iptr_A_1_imag, // @[:@6.4]
  input  [15:0] io_iptr_A_2_real, // @[:@6.4]
  input  [15:0] io_iptr_A_2_imag, // @[:@6.4]
  input  [15:0] io_iptr_A_3_real, // @[:@6.4]
  input  [15:0] io_iptr_A_3_imag, // @[:@6.4]
  output [15:0] io_Z_0_real, // @[:@6.4]
  output [15:0] io_Z_0_imag, // @[:@6.4]
  output [15:0] io_Z_1_real, // @[:@6.4]
  output [15:0] io_Z_1_imag, // @[:@6.4]
  output [15:0] io_Z_2_real, // @[:@6.4]
  output [15:0] io_Z_2_imag, // @[:@6.4]
  output [15:0] io_Z_3_real, // @[:@6.4]
  output [15:0] io_Z_3_imag // @[:@6.4]
);
  reg [15:0] _T_60_0_real; // @[f2_digital_postproc_tapein0.scala 18:18:@8.4]
  reg [31:0] _RAND_0;
  reg [15:0] _T_60_0_imag; // @[f2_digital_postproc_tapein0.scala 18:18:@8.4]
  reg [31:0] _RAND_1;
  reg [15:0] _T_60_1_real; // @[f2_digital_postproc_tapein0.scala 18:18:@8.4]
  reg [31:0] _RAND_2;
  reg [15:0] _T_60_1_imag; // @[f2_digital_postproc_tapein0.scala 18:18:@8.4]
  reg [31:0] _RAND_3;
  reg [15:0] _T_60_2_real; // @[f2_digital_postproc_tapein0.scala 18:18:@8.4]
  reg [31:0] _RAND_4;
  reg [15:0] _T_60_2_imag; // @[f2_digital_postproc_tapein0.scala 18:18:@8.4]
  reg [31:0] _RAND_5;
  reg [15:0] _T_60_3_real; // @[f2_digital_postproc_tapein0.scala 18:18:@8.4]
  reg [31:0] _RAND_6;
  reg [15:0] _T_60_3_imag; // @[f2_digital_postproc_tapein0.scala 18:18:@8.4]
  reg [31:0] _RAND_7;
  assign io_Z_0_real = _T_60_0_real;
  assign io_Z_0_imag = _T_60_0_imag;
  assign io_Z_1_real = _T_60_1_real;
  assign io_Z_1_imag = _T_60_1_imag;
  assign io_Z_2_real = _T_60_2_real;
  assign io_Z_2_imag = _T_60_2_imag;
  assign io_Z_3_real = _T_60_3_real;
  assign io_Z_3_imag = _T_60_3_imag;
`ifdef RANDOMIZE
  integer initvar;
  initial begin
    `ifndef verilator
      #0.002 begin end
    `endif
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{$random}};
  _T_60_0_real = _RAND_0[15:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{$random}};
  _T_60_0_imag = _RAND_1[15:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{$random}};
  _T_60_1_real = _RAND_2[15:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{$random}};
  _T_60_1_imag = _RAND_3[15:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{$random}};
  _T_60_2_real = _RAND_4[15:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{$random}};
  _T_60_2_imag = _RAND_5[15:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{$random}};
  _T_60_3_real = _RAND_6[15:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{$random}};
  _T_60_3_imag = _RAND_7[15:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    _T_60_0_real <= io_iptr_A_0_real;
    _T_60_0_imag <= io_iptr_A_0_imag;
    _T_60_1_real <= io_iptr_A_1_real;
    _T_60_1_imag <= io_iptr_A_1_imag;
    _T_60_2_real <= io_iptr_A_2_real;
    _T_60_2_imag <= io_iptr_A_2_imag;
    _T_60_3_real <= io_iptr_A_3_real;
    _T_60_3_imag <= io_iptr_A_3_imag;
  end
endmodule
