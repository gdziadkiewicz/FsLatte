
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)

@0 = internal constant [1 x i8] c"\00"
@1 = internal constant [2 x i8] c"=\00"
@2 = internal constant [9 x i8] c"hello */\00"
@3 = internal constant [9 x i8] c"/* world\00"
define i32 @main () {
%v1 = call i32 @fac(i32 10)
call void @printInt(i32 %v1)
%v3 = call i32 @rfac(i32 10)
call void @printInt(i32 %v3)
%v5 = call i32 @mfac(i32 10)
call void @printInt(i32 %v5)
%v7 = call i32 @ifac(i32 10)
call void @printInt(i32 %v7)
%v9 = getelementptr [1 x i8], [1 x i8]* @0, i32 0, i32 0
%v10 = alloca i8* 
store i8* %v9, i8** %v10
%v11 = alloca i32 
store i32 10, i32* %v11
%v12 = alloca i32 
store i32 1, i32* %v12
br label %v15
v15:
%v13 = load i32, i32* %v11
%v14 = icmp sgt i32 %v13, 0
br i1 %v14, label %v16, label %v17
v16:
%v18 = load i32, i32* %v12
%v19 = load i32, i32* %v11
%v20 = mul i32 %v18, %v19
store i32 %v20, i32* %v12
%v21 = load i32, i32* %v11
%v22 = sub i32 %v21, 1
store i32 %v22, i32* %v11
br label %v15
v17:
%v23 = load i32, i32* %v12
call void @printInt(i32 %v23)
%v25 = getelementptr [2 x i8], [2 x i8]* @1, i32 0, i32 0
%v26 = call i8* @repStr(i8* %v25, i32 60)
call void @printString(i8* %v26)
%v28 = getelementptr [9 x i8], [9 x i8]* @2, i32 0, i32 0
call void @printString(i8* %v28)
%v30 = getelementptr [9 x i8], [9 x i8]* @3, i32 0, i32 0
call void @printString(i8* %v30)
ret i32 0
}
define i32 @fac (i32 %v1) {
%v2 = alloca i32 
store i32 %v1, i32* %v2
%v3 = alloca i32 
store i32 0, i32* %v3
%v4 = alloca i32 
store i32 0, i32* %v4
store i32 1, i32* %v3
%v5 = load i32, i32* %v2
store i32 %v5, i32* %v4
br label %v8
v8:
%v6 = load i32, i32* %v4
%v7 = icmp sgt i32 %v6, 0
br i1 %v7, label %v9, label %v10
v9:
%v11 = load i32, i32* %v3
%v12 = load i32, i32* %v4
%v13 = mul i32 %v11, %v12
store i32 %v13, i32* %v3
%v14 = load i32, i32* %v4
%v15 = sub i32 %v14, 1
store i32 %v15, i32* %v4
br label %v8
v10:
%v16 = load i32, i32* %v3
ret i32 %v16
}
define i32 @rfac (i32 %v1) {
%v2 = alloca i32 
store i32 %v1, i32* %v2
%v3 = load i32, i32* %v2
%v4 = icmp eq i32 %v3, 0
br i1 %v4, label %v5, label %v6
v5:
ret i32 1
br label %v7
v6:
%v8 = load i32, i32* %v2
%v9 = load i32, i32* %v2
%v10 = sub i32 %v9, 1
%v11 = call i32 @rfac(i32 %v10)
%v12 = mul i32 %v8, %v11
ret i32 %v12
br label %v7
v7:
ret i32 0
}
define i32 @mfac (i32 %v1) {
%v2 = alloca i32 
store i32 %v1, i32* %v2
%v3 = load i32, i32* %v2
%v4 = icmp eq i32 %v3, 0
br i1 %v4, label %v5, label %v6
v5:
ret i32 1
br label %v7
v6:
%v8 = load i32, i32* %v2
%v9 = load i32, i32* %v2
%v10 = sub i32 %v9, 1
%v11 = call i32 @nfac(i32 %v10)
%v12 = mul i32 %v8, %v11
ret i32 %v12
br label %v7
v7:
ret i32 0
}
define i32 @nfac (i32 %v1) {
%v2 = alloca i32 
store i32 %v1, i32* %v2
%v3 = load i32, i32* %v2
%v4 = icmp ne i32 %v3, 0
br i1 %v4, label %v5, label %v6
v5:
%v8 = load i32, i32* %v2
%v9 = sub i32 %v8, 1
%v10 = call i32 @mfac(i32 %v9)
%v11 = load i32, i32* %v2
%v12 = mul i32 %v10, %v11
ret i32 %v12
br label %v7
v6:
ret i32 1
br label %v7
v7:
ret i32 0
}
define i32 @ifac (i32 %v1) {
%v2 = alloca i32 
store i32 %v1, i32* %v2
%v3 = load i32, i32* %v2
%v4 = call i32 @ifac2f(i32 1, i32 %v3)
ret i32 %v4
}
define i32 @ifac2f (i32 %v1, i32 %v2) {
%v3 = alloca i32 
store i32 %v1, i32* %v3
%v4 = alloca i32 
store i32 %v2, i32* %v4
%v5 = load i32, i32* %v3
%v6 = load i32, i32* %v4
%v7 = icmp eq i32 %v5, %v6
br i1 %v7, label %v8, label %v9
v8:
%v10 = load i32, i32* %v3
ret i32 %v10
br label %v9
v9:
%v11 = load i32, i32* %v3
%v12 = load i32, i32* %v4
%v13 = icmp sgt i32 %v11, %v12
br i1 %v13, label %v14, label %v15
v14:
ret i32 1
br label %v15
v15:
%v16 = alloca i32 
store i32 0, i32* %v16
%v17 = load i32, i32* %v3
%v18 = load i32, i32* %v4
%v19 = add i32 %v17, %v18
%v20 = sdiv i32 %v19, 2
store i32 %v20, i32* %v16
%v21 = load i32, i32* %v3
%v22 = load i32, i32* %v16
%v23 = call i32 @ifac2f(i32 %v21, i32 %v22)
%v24 = load i32, i32* %v16
%v25 = add i32 %v24, 1
%v26 = load i32, i32* %v4
%v27 = call i32 @ifac2f(i32 %v25, i32 %v26)
%v28 = mul i32 %v23, %v27
ret i32 %v28
}
define i8* @repStr (i8* %v1, i32 %v2) {
%v3 = alloca i8* 
store i8* %v1, i8** %v3
%v4 = alloca i32 
store i32 %v2, i32* %v4
%v5 = getelementptr [1 x i8], [1 x i8]* @0, i32 0, i32 0
%v6 = alloca i8* 
store i8* %v5, i8** %v6
%v7 = alloca i32 
store i32 0, i32* %v7
br label %v11
v11:
%v8 = load i32, i32* %v7
%v9 = load i32, i32* %v4
%v10 = icmp slt i32 %v8, %v9
br i1 %v10, label %v12, label %v13
v12:
%v14 = load i8*, i8** %v6
%v15 = load i8*, i8** %v3
%v16 = call i8* @concat(i8* %v14, i8* %v15)
store i8* %v16, i8** %v6
%v17 = load i32, i32* %v7
%v18 = add i32 %v17, 1
store i32 %v18, i32* %v7
br label %v11
v13:
%v19 = load i8*, i8** %v6
ret i8* %v19
}