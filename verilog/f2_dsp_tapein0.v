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
  input  [15:0] io_iptr_A_real, // @[:@6.4]
  input  [15:0] io_iptr_A_imag, // @[:@6.4]
  output [15:0] io_Z_real, // @[:@6.4]
  output [15:0] io_Z_imag // @[:@6.4]
);
  reg [15:0] inreg_real; // @[f2_dsp_tapein0.scala 18:23:@8.4]
  reg [31:0] _RAND_0;
  reg [15:0] inreg_imag; // @[f2_dsp_tapein0.scala 18:23:@8.4]
  reg [31:0] _RAND_1;
  reg [15:0] _T_18_real; // @[f2_dsp_tapein0.scala 21:20:@11.4]
  reg [31:0] _RAND_2;
  reg [15:0] _T_18_imag; // @[f2_dsp_tapein0.scala 21:20:@11.4]
  reg [31:0] _RAND_3;
  assign io_Z_real = _T_18_real;
  assign io_Z_imag = _T_18_imag;
`ifdef RANDOMIZE
  integer initvar;
  initial begin
    `ifndef verilator
      #0.002 begin end
    `endif
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{$random}};
  inreg_real = _RAND_0[15:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{$random}};
  inreg_imag = _RAND_1[15:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{$random}};
  _T_18_real = _RAND_2[15:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{$random}};
  _T_18_imag = _RAND_3[15:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    inreg_real <= io_iptr_A_real;
    inreg_imag <= io_iptr_A_imag;
  end
  always @(posedge io_clock_DSP) begin
    _T_18_real <= inreg_real;
    _T_18_imag <= inreg_imag;
  end
endmodule
