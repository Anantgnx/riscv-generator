package core

import chisel3._

case class Config(
                   xLen: Int = 32,
                   numRegs: Int = 32,
                   isThreeStage: Boolean = false,

                   hasMul: Boolean = false,
                   hasDiv: Boolean = false,
                   mulLatency: Int = 1,

                   hasCache: Boolean = false,
                   cacheAssociativity: Int = 1,
                   cacheSizeKB: Int = 1
                 )

