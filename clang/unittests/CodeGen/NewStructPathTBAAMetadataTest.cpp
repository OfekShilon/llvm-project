//=== unittests/CodeGen/NewStructPathTBAAMetadataTest.cpp - Checks metadata generation -===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "IRMatchers.h"
#include "TestCompiler.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/MemoryBuffer.h"
#include "gtest/gtest.h"
#include <memory>

using namespace llvm;

namespace {

struct TBAATestCompiler : public TestCompiler {
  TBAATestCompiler(clang::LangOptions LO, clang::CodeGenOptions CGO)
    : TestCompiler(LO, CGO) {}
  static clang::CodeGenOptions getCodeGenOpts() {
    clang::CodeGenOptions CGOpts;
    CGOpts.StructPathTBAA = 1;
    CGOpts.NewStructPathTBAA = 1;  
    CGOpts.OptimizationLevel = 1;
    return CGOpts;
  }
};


auto RootC   = MMTuple(MMString("Simple C/C++ TBAA"));
auto RootCXX = MMTuple(MMString("Simple C++ TBAA"));

auto OmnipotentCharC   = MMTuple(RootC,   MConstInt(1, 64), MMString("omnipotent char"));
auto OmnipotentCharCXX = MMTuple(RootCXX, MConstInt(1, 64), MMString("omnipotent char"));

auto ShortC   = MMTuple(OmnipotentCharC,   MConstInt(2), MMString("short"));
auto ShortCXX = MMTuple(OmnipotentCharCXX, MConstInt(2), MMString("short"));
  
auto IntC   = MMTuple(OmnipotentCharC,   MConstInt(4), MMString("int"));  
auto IntCXX = MMTuple(OmnipotentCharCXX, MConstInt(4), MMString("int"));
  
auto LongLongC   = MMTuple(OmnipotentCharC,   MConstInt(8), MMString("long long"));
auto LongLongCXX = MMTuple(OmnipotentCharCXX, MConstInt(8), MMString("long long"));

TEST(NewStructPathTBAAMetadataTest, BasicTypes) {
  const char TestProgram[] = R"**(
    void func(char *CP, short *SP, int *IP, long long *LP, void **VPP,
              int **IPP) {
      *CP = 4;
      *SP = 11;
      *IP = 601;
      *LP = 604;
      *VPP = CP;
      *IPP = IP;
    }
  )**";

  clang::LangOptions LO;
  TBAATestCompiler Compiler(LO, TBAATestCompiler::getCodeGenOpts());
  Compiler.init(TestProgram);
  const BasicBlock *BB = Compiler.compile();

  const Instruction *I = match(BB,
      MInstruction(Instruction::Store,
        MConstInt(4, 8),
        MMTuple(
          OmnipotentCharC,
          MSameAs(0),
          MConstInt(0),
          MConstInt(1))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(11, 16),
        MMTuple(
          ShortC,
          MSameAs(0),
          MConstInt(0),
          MConstInt(2))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(601, 32),
        MMTuple(
          IntC,
          MSameAs(0),
          MConstInt(0),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(604, 64),
        MMTuple(
          LongLongC,
          MSameAs(0),
          MConstInt(0),
          MConstInt(8))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MValType(Type::getInt8PtrTy(Compiler.Context)),
        MMTuple(
          MMTuple(
            OmnipotentCharC,
            MConstInt(8),
            MMString("any pointer")),
          MSameAs(0),
          MConstInt(0),
          MConstInt(8))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MValType(Type::getInt32PtrTy(Compiler.Context)),
        MMTuple(
          MMTuple(
            OmnipotentCharC,
            MConstInt(8),
            MMString("any pointer")),
          MSameAs(0),
          MConstInt(0),
          MConstInt(8))));
  ASSERT_TRUE(I);
}

TEST(NewStructPathTBAAMetadataTest, CFields) {
  const char TestProgram[] = R"**(
    struct ABC {
       short f16;
       int f32;
       long long f64;
       unsigned short f16_2;
       unsigned f32_2;
       unsigned long long f64_2;
    };

    void func(struct ABC *A) {
      A->f32 = 4;
      A->f16 = 11;
      A->f64 = 601;
      A->f16_2 = 22;
      A->f32_2 = 77;
      A->f64_2 = 604;
    }
  )**";

  clang::LangOptions LO;
  LO.C11 = 1;
  TBAATestCompiler Compiler(LO, TBAATestCompiler::getCodeGenOpts());
  Compiler.init(TestProgram);
  const BasicBlock *BB = Compiler.compile();


  auto StructABC = MMTuple(
    OmnipotentCharC,
    MConstInt(32),
    MMString("ABC"),
    ShortC,
    MConstInt(0),
    MConstInt(2),
    IntC,
    MConstInt(4),
    MConstInt(4),
    LongLongC,
    MConstInt(8),
    MConstInt(8),
    MSameAs(3),
    MConstInt(16),
    MConstInt(2),
    MSameAs(6),
    MConstInt(20),
    MConstInt(4),
    MSameAs(9),
    MConstInt(24),
    MConstInt(8));

  const Instruction *I = match(BB,
      MInstruction(Instruction::Store,
        MConstInt(4, 32),
        MMTuple(
          StructABC,
          IntC,
          MConstInt(4),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(11, 16),
        MMTuple(
          StructABC,
          ShortC,
          MConstInt(0),
          MConstInt(2))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(601, 64),
        MMTuple(
          StructABC,
          LongLongC,
          MConstInt(8),
          MConstInt(8))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(22, 16),
        MMTuple(
          StructABC,
          ShortC,
          MConstInt(16),
          MConstInt(2))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(77, 32),
        MMTuple(
          StructABC,
          IntC,
          MConstInt(20),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(604, 64),
        MMTuple(
          StructABC,
          LongLongC,
          MConstInt(24),
          MConstInt(8))));
  ASSERT_TRUE(I);
}

TEST(NewStructPathTBAAMetadataTest, CTypedefFields) {
  const char TestProgram[] = R"**(
    typedef struct {
       short f16;
       int f32;
    } ABC;
    typedef struct {
       short value_f16;
       int value_f32;
    } CDE;

    void func(ABC *A, CDE *B) {
      A->f32 = 4;
      A->f16 = 11;
      B->value_f32 = 44;
      B->value_f16 = 111;
    }
  )**";

  clang::LangOptions LO;
  LO.C11 = 1;
  TBAATestCompiler Compiler(LO, TBAATestCompiler::getCodeGenOpts());
  Compiler.init(TestProgram);
  const BasicBlock *BB = Compiler.compile();

  auto NamelessStruct = MMTuple(
    OmnipotentCharC,
    MConstInt(8),
    MMString(""),
    ShortC,
    MConstInt(0),
    MConstInt(2),
    IntC,
    MConstInt(4),
    MConstInt(4));

  const Metadata *MetaABC = nullptr;
  const Instruction *I = match(BB,
      MInstruction(Instruction::Store,
        MConstInt(4, 32),
        MMTuple(
          MMSave(MetaABC, NamelessStruct),
          IntC,
          MConstInt(4),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(11, 16),
        MMTuple(
          NamelessStruct,
          ShortC,
          MConstInt(0),
          MConstInt(2))));
  ASSERT_TRUE(I);

  const Metadata *MetaCDE = nullptr;
  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(44, 32),
        MMTuple(
          MMSave(MetaCDE, NamelessStruct),
          IntC,
          MConstInt(4),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(111, 16),
        MMTuple(
          NamelessStruct,
          ShortC,
          MConstInt(0),
          MConstInt(2))));
  ASSERT_TRUE(I);

  // FIXME: Nameless structures used in definitions of 'ABC' and 'CDE' are
  // different structures and must be described by different descriptors.
  //ASSERT_TRUE(MetaABC != MetaCDE);
}

TEST(NewStructPathTBAAMetadataTest, CTypedefFields2) {
  const char TestProgram[] = R"**(
    typedef struct {
       short f16;
       int f32;
    } ABC;
    typedef struct {
       short f16;
       int f32;
    } CDE;

    void func(ABC *A, CDE *B) {
      A->f32 = 4;
      A->f16 = 11;
      B->f32 = 44;
      B->f16 = 111;
    }
  )**";

  clang::LangOptions LO;
  LO.C11 = 1;
  TBAATestCompiler Compiler(LO, TBAATestCompiler::getCodeGenOpts());
  Compiler.init(TestProgram);
  const BasicBlock *BB = Compiler.compile();

    auto NamelessStruct = MMTuple(
    OmnipotentCharC,
    MConstInt(8),
    MMString(""),
    ShortC,
    MConstInt(0),
    MConstInt(2),
    IntC,
    MConstInt(4),
    MConstInt(4));

  const Metadata *MetaABC = nullptr;
  const Instruction *I = match(BB,
      MInstruction(Instruction::Store,
        MConstInt(4, 32),
        MMTuple(
          MMSave(MetaABC, NamelessStruct),
          IntC,
          MConstInt(4),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(11, 16),
        MMTuple(
          NamelessStruct,
          ShortC,
          MConstInt(0),
          MConstInt(2))));
  ASSERT_TRUE(I);

  const Metadata *MetaCDE = nullptr;
  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(44, 32),
        MMTuple(
          MMSave(MetaCDE, NamelessStruct),
          IntC,
          MConstInt(4),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(111, 16),
        MMTuple(
          NamelessStruct,
          ShortC,
          MConstInt(0),
          MConstInt(2))));
  ASSERT_TRUE(I);

  // FIXME: Nameless structures used in definitions of 'ABC' and 'CDE' are
  // different structures, although they have the same field sequence. They must
  // be described by different descriptors.
  //ASSERT_TRUE(MetaABC != MetaCDE);
}

TEST(NewStructPathTBAAMetadataTest, CTypedefFields3) {
  const char TestProgram[] = R"**(
    typedef struct {
       short f16;
       int f32;
    } ABC;
    typedef struct {
       int f32;
       short f16;
    } CDE;

    void func(ABC *A, CDE *B) {
      A->f32 = 4;
      A->f16 = 11;
      B->f32 = 44;
      B->f16 = 111;
    }
  )**";

  clang::LangOptions LO;
  LO.C11 = 1;
  TBAATestCompiler Compiler(LO, TBAATestCompiler::getCodeGenOpts());
  Compiler.init(TestProgram);
  const BasicBlock *BB = Compiler.compile();

  auto NamelessStruct1 = MMTuple(
    OmnipotentCharC,
    MConstInt(8),
    MMString(""),
    ShortC,
    MConstInt(0),
    MConstInt(2),
    IntC,
    MConstInt(4),
    MConstInt(4));

  auto NamelessStruct2 = MMTuple(
    OmnipotentCharC,
    MConstInt(8),
    MMString(""),
    IntC,
    MConstInt(0),
    MConstInt(4),
    ShortC,
    MConstInt(4),
    MConstInt(2));

  const Instruction *I = match(BB,
      MInstruction(Instruction::Store,
        MConstInt(4, 32),
        MMTuple(
          NamelessStruct1,
          IntC,
          MConstInt(4),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(11, 16),
        MMTuple(
          NamelessStruct1,
          ShortC,
          MConstInt(0),
          MConstInt(2))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(44, 32),
        MMTuple(
          NamelessStruct2,
          IntC,
          MConstInt(0),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(111, 16),
        MMTuple(
          NamelessStruct2,
          ShortC,
          MConstInt(4),
          MConstInt(2))));
  ASSERT_TRUE(I);
}

TEST(NewStructPathTBAAMetadataTest, CXXFields) {
  const char TestProgram[] = R"**(
    struct ABC {
       short f16;
       int f32;
       long long f64;
       unsigned short f16_2;
       unsigned f32_2;
       unsigned long long f64_2;
    };

    void func(struct ABC *A) {
      A->f32 = 4;
      A->f16 = 11;
      A->f64 = 601;
      A->f16_2 = 22;
      A->f32_2 = 77;
      A->f64_2 = 604;
    }
  )**";

  clang::LangOptions LO;
  LO.CPlusPlus = 1;
  LO.CPlusPlus11 = 1;
  TBAATestCompiler Compiler(LO, TBAATestCompiler::getCodeGenOpts());
  Compiler.init(TestProgram);
  const BasicBlock *BB = Compiler.compile();

  auto StructABC = MMTuple(
    OmnipotentCharCXX,
    MConstInt(32), 
    MMString("_ZTS3ABC"),
    ShortCXX,
    MConstInt(0),
    MConstInt(2),
    IntCXX,
    MConstInt(4),
    MConstInt(4),
    LongLongCXX,
    MConstInt(8),
    MConstInt(8),
    ShortCXX,
    MConstInt(16),
    MConstInt(2),
    IntCXX,
    MConstInt(20),
    MConstInt(4),
    LongLongCXX,
    MConstInt(24),
    MConstInt(8));

  const Instruction *I = match(BB,
      MInstruction(Instruction::Store,
        MConstInt(4, 32),
        MMTuple(
          StructABC,
          IntCXX,
          MConstInt(4),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(11, 16),
        MMTuple(
          StructABC,
          ShortCXX,
          MConstInt(0),
          MConstInt(2))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(601, 64),
        MMTuple(
          StructABC,
          LongLongCXX,
          MConstInt(8),
          MConstInt(8))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(22, 16),
        MMTuple(
          StructABC,
          ShortCXX,
          MConstInt(16),
          MConstInt(2))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(77, 32),
        MMTuple(
          StructABC,
          IntCXX,
          MConstInt(20),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(604, 64),
        MMTuple(
          StructABC,
          LongLongCXX,
          MConstInt(24),
          MConstInt(8))));
  ASSERT_TRUE(I);
}

TEST(NewStructPathTBAAMetadataTest, CXXTypedefFields) {
  const char TestProgram[] = R"**(
    typedef struct {
       short f16;
       int f32;
    } ABC;
    typedef struct {
       short value_f16;
       int value_f32;
    } CDE;

    void func(ABC *A, CDE *B) {
      A->f32 = 4;
      A->f16 = 11;
      B->value_f32 = 44;
      B->value_f16 = 111;
    }
  )**";

  clang::LangOptions LO;
  LO.CPlusPlus = 1;
  LO.CPlusPlus11 = 1;
  TBAATestCompiler Compiler(LO, TBAATestCompiler::getCodeGenOpts());
  Compiler.init(TestProgram);
  const BasicBlock *BB = Compiler.compile();

  auto StructABC = MMTuple(
    OmnipotentCharCXX, 
    MConstInt(8),
    MMString("_ZTS3ABC"),
    ShortCXX,
    MConstInt(0),
    MConstInt(2),
    IntCXX,
    MConstInt(4),
    MConstInt(4));

  auto StructCDE = MMTuple(
    OmnipotentCharCXX, 
    MConstInt(8),
    MMString("_ZTS3CDE"),
    ShortCXX,
    MConstInt(0),
    MConstInt(2),
    IntCXX,
    MConstInt(4),
    MConstInt(4));

  const Instruction *I = match(BB,
      MInstruction(Instruction::Store,
        MConstInt(4, 32),
        MMTuple(
          StructABC,
          IntCXX,
          MConstInt(4),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(11, 16),
        MMTuple(
          StructABC,
          ShortCXX,
          MConstInt(0),
          MConstInt(2))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(44, 32),
        MMTuple(
          StructCDE,
          IntCXX,
          MConstInt(4),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(111, 16),
        MMTuple(
          StructCDE,
          ShortCXX,
          MConstInt(0),
          MConstInt(2))));
  ASSERT_TRUE(I);
}

TEST(NewStructPathTBAAMetadataTest, StructureFields) {
  const char TestProgram[] = R"**(
    struct Inner {
      int f32;
    };

    struct Outer {
      short f16;
      Inner b1;
      Inner b2;
    };

    void func(Outer *S) {
      S->f16 = 14;
      S->b1.f32 = 35;
      S->b2.f32 = 77;
    }
  )**";

  clang::LangOptions LO;
  LO.CPlusPlus = 1;
  LO.CPlusPlus11 = 1;
  TBAATestCompiler Compiler(LO, TBAATestCompiler::getCodeGenOpts());
  Compiler.init(TestProgram);
  const BasicBlock *BB = Compiler.compile();

  auto StructInner = MMTuple(
    OmnipotentCharCXX,
    MConstInt(4),
    MMString("_ZTS5Inner"),
    IntCXX,
    MConstInt(0),
    MConstInt(4));

  auto StructOuter = MMTuple(
    OmnipotentCharCXX,
    MConstInt(12),
    MMString("_ZTS5Outer"),
    ShortCXX,
    MConstInt(0),
    MConstInt(2),
    StructInner,
    MConstInt(4),
    MConstInt(4),
    StructInner,
    MConstInt(8),
    MConstInt(4));

  const Instruction *I = match(BB,
      MInstruction(Instruction::Store,
        MConstInt(14, 16),
        MMTuple(
          StructOuter,
          ShortCXX,
          MConstInt(0),
          MConstInt(2))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(35, 32),
        MMTuple(
          StructOuter,
          IntCXX,
          MConstInt(4),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(77, 32),
        MMTuple(
          StructOuter,
          IntCXX,
          MConstInt(8),
          MConstInt(4))));
  ASSERT_TRUE(I);
}

TEST(NewStructPathTBAAMetadataTest, ArrayFields) {
  const char TestProgram[] = R"**(
    struct Inner {
      int f32;
    };

    struct Outer {
      short f16;
      Inner b1[2];
    };

    void func(Outer *S) {
      S->f16 = 14;
      S->b1[0].f32 = 35;
      S->b1[1].f32 = 77;
    }
  )**";

  clang::LangOptions LO;
  LO.CPlusPlus = 1;
  LO.CPlusPlus11 = 1;
  TBAATestCompiler Compiler(LO, TBAATestCompiler::getCodeGenOpts());
  Compiler.init(TestProgram);
  const BasicBlock *BB = Compiler.compile();

  auto StructInner = MMTuple(
    OmnipotentCharCXX,
    MConstInt(4),
    MMString("_ZTS5Inner"),
    IntCXX,
    MConstInt(0),
    MConstInt(4));

  auto StructOuter = MMTuple(
    OmnipotentCharCXX,
    MConstInt(12),
    MMString("_ZTS5Outer"),
    ShortCXX,
    MConstInt(0),
    MConstInt(2),
    StructInner,
    MConstInt(4),
    MConstInt(8));

  const Instruction *I = match(BB,
      MInstruction(Instruction::Store,
        MConstInt(14, 16),
        MMTuple(
          StructOuter,
          ShortCXX,
          MConstInt(0),
          MConstInt(2))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(35, 32),
        MMTuple(
          StructInner,
          IntCXX,
          MConstInt(0),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(77, 32),
        MMTuple(
          StructInner,
          IntCXX,
          MConstInt(0),
          MConstInt(4))));
  ASSERT_TRUE(I);
}

TEST(NewStructPathTBAAMetadataTest, BaseClass) {
  const char TestProgram[] = R"**(
    struct Base {
      int f32;
    };

    struct Derived : public Base {
      short f16;
    };

    void func(Base *B, Derived *D) {
      B->f32 = 14;
      D->f16 = 35;
      D->f32 = 77;
    }
  )**";

  clang::LangOptions LO;
  LO.CPlusPlus = 1;
  LO.CPlusPlus11 = 1;
  TBAATestCompiler Compiler(LO, TBAATestCompiler::getCodeGenOpts());
  Compiler.init(TestProgram);
  const BasicBlock *BB = Compiler.compile();

  auto ClassBase = MMTuple(
    OmnipotentCharCXX,
    MConstInt(4),
    MMString("_ZTS4Base"),
    IntCXX,
    MConstInt(0),
    MConstInt(4));

  auto ClassDerived = MMTuple(
    OmnipotentCharCXX,
    MConstInt(8),
    MMString("_ZTS7Derived"), 
    ClassBase, 
    MConstInt(0),
    MConstInt(4),
    ShortCXX,
    MConstInt(4),
    MConstInt(2));

  const Instruction *I = match(BB,
      MInstruction(Instruction::Store,
        MConstInt(14, 32),
        MMTuple(
          ClassBase,
          IntCXX,
          MConstInt(0),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(35, 16),
        MMTuple(
          ClassDerived,
          ShortCXX,
          MConstInt(4),
          MConstInt(2))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(77, 32),
        MMTuple(
          ClassBase,
          IntCXX,
          MConstInt(0),
          MConstInt(4))));
  ASSERT_TRUE(I);
}

TEST(NewStructPathTBAAMetadataTest, PolymorphicClass) {
  const char TestProgram[] = R"**(
    struct Base {
      virtual void m1(int *) = 0;
      int f32;
    };

    struct Derived : public Base {
      virtual void m1(int *) override;
      short f16;
    };

    void func(Base *B, Derived *D) {
      B->f32 = 14;
      D->f16 = 35;
      D->f32 = 77;
    }
  )**";

  clang::LangOptions LO;
  LO.CPlusPlus = 1;
  LO.CPlusPlus11 = 1;
  TBAATestCompiler Compiler(LO, TBAATestCompiler::getCodeGenOpts());
  Compiler.init(TestProgram);
  const BasicBlock *BB = Compiler.compile();

  auto ClassBase = MMTuple(
    OmnipotentCharCXX,
    MConstInt(16),
    MMString("_ZTS4Base"),
    IntCXX,
    MConstInt(Compiler.PtrSize),
    MConstInt(4));

  auto ClassDerived = MMTuple(
    OmnipotentCharCXX,
    MConstInt(16),
    MMString("_ZTS7Derived"), 
    ClassBase, 
    MConstInt(0),
    MConstInt(12),
    ShortCXX,
    MConstInt(Compiler.PtrSize + 4),
    MConstInt(2));

  const Instruction *I = match(BB,
      MInstruction(Instruction::Store,
        MConstInt(14, 32),
        MMTuple(
          ClassBase,
          IntCXX,
          MConstInt(Compiler.PtrSize),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(35, 16),
        MMTuple(
          ClassDerived,
          ShortCXX,
          MConstInt(Compiler.PtrSize + 4),
          MConstInt(2))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(77, 32),
        MMTuple(
          ClassBase,
          IntCXX,
          MConstInt(Compiler.PtrSize),
          MConstInt(4))));
  ASSERT_TRUE(I);
}

TEST(NewStructPathTBAAMetadataTest, VirtualBase) {
  const char TestProgram[] = R"**(
    struct Base {
      int f32;
    };

    struct Derived : public virtual Base {
      short f16;
    };

    void func(Base *B, Derived *D) {
      B->f32 = 14;
      D->f16 = 35;
      D->f32 = 77;
    }
  )**";

  clang::LangOptions LO;
  LO.CPlusPlus = 1;
  LO.CPlusPlus11 = 1;
  TBAATestCompiler Compiler(LO, TBAATestCompiler::getCodeGenOpts());
  Compiler.init(TestProgram);
  const BasicBlock *BB = Compiler.compile();

  auto ClassBase = MMTuple(
    OmnipotentCharCXX,
    MConstInt(4),
    MMString("_ZTS4Base"),
    IntCXX,
    MConstInt(0),
    MConstInt(4));

  auto ClassDerived = MMTuple(    // <--- !!
    MMString("_ZTS7Derived"),
    ShortC,
    MConstInt(Compiler.PtrSize));

  const Instruction *I = match(BB,
      MInstruction(Instruction::Store,
        MConstInt(14, 32),
        MMTuple(
          ClassBase,
          IntCXX,
          MConstInt(0),
          MConstInt(4))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(35, 16),
        MMTuple(
          ShortCXX, //  ClassDerived,  <--- !!
          ShortCXX,
          MConstInt(0),
          MConstInt(2))));
          // MConstInt(Compiler.PtrSize))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Load,
        MMTuple(
          MMTuple(
            RootCXX,
            MConstInt(8),
            MMString("vtable pointer")),
          MSameAs(0),
          MConstInt(0),
          MConstInt(8))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(77, 32),
        MMTuple(
          ClassBase,
          IntCXX,
          MConstInt(0),
          MConstInt(4))));
  ASSERT_TRUE(I);
}

TEST(NewStructPathTBAAMetadataTest, TemplSpec) {
  const char TestProgram[] = R"**(
    template<typename T1, typename T2>
    struct ABC {
      T1 f1;
      T2 f2;
    };

    void func(ABC<double, int> *p) {
      p->f1 = 12.1;
      p->f2 = 44;
    }
  )**";

  clang::LangOptions LO;
  LO.CPlusPlus = 1;
  LO.CPlusPlus11 = 1;
  TBAATestCompiler Compiler(LO, TBAATestCompiler::getCodeGenOpts());
  Compiler.init(TestProgram);
  const BasicBlock *BB = Compiler.compile();

  auto SpecABC = MMTuple(
    OmnipotentCharCXX,
    MConstInt(16),
    MMString("_ZTS3ABCIdiE"),
    MMTuple(
      OmnipotentCharCXX,
      MConstInt(8),
      MMString("double")),
    MConstInt(0),
    MConstInt(8),
    IntCXX,
    MConstInt(8),
    MConstInt(4));

  const Instruction *I = match(BB,
      MInstruction(Instruction::Store,
        MMTuple(
          SpecABC,
        MMTuple(
          OmnipotentCharCXX,
          MConstInt(8),
          MMString("double")),
        MConstInt(0),
        MConstInt(8))));
  ASSERT_TRUE(I);

  I = matchNext(I,
      MInstruction(Instruction::Store,
        MConstInt(44, 32),
        MMTuple(
          SpecABC,
          IntCXX,
          MConstInt(8),
          MConstInt(4))));
  ASSERT_TRUE(I);
}
}
