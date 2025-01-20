target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }

@stdin = external global %struct._IO_FILE*, align 8
@.str.1 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str.2 = private unnamed_addr constant [3 x i8] c"\0D\0A\00", align 1
declare i8* @malloc(i64) #1
declare i64 @strlen(i8*) #1
declare i8* @strcat(i8*, i8*) #1
declare i8* @strcpy(i8*, i8*) #1
declare i64 @strcspn(i8*, i8*) #3

define i8* @concat(i8* %s1, i8* %s2) {
  %1 = call i64 @strlen(i8* %s1)
  %2 = call i64 @strlen(i8* %s2)
  %3 = add i64 %1, 1
  %4 = add i64 %3, %2
  %5 = call i8* @malloc(i64 %4)
  %6 = call i8* @strcpy(i8* %5, i8* %s1)
  %7 = call i8* @strcat(i8* %6, i8* %s2)
  ret i8* %7
}


define i8* @readString() #0 {
  %required_size = alloca i32, align 4
  %str_to_ret = alloca i8*, align 8
  store i32 256, i32* %required_size, align 4
  %1 = load  i32* %required_size, align 4
  %2 = sext i32 %1 to i64
  %3 = mul i64 1, %2
  %4 = call noalias i8* @malloc(i64 %3) #4
  store i8* %4, i8** %str_to_ret, align 8
  %5 = load i8** %str_to_ret, align 8
  %6 = load %struct._IO_FILE** @stdin, align 8
  %7 = call i8* @fgets(i8* %5, i32 8, %struct._IO_FILE* %6)
  %8 = load i8** %str_to_ret, align 8
  %9 = call i64 @strcspn(i8* %8, i8* getelementptr inbounds ( [3 x i8]* @.str.2, i32 0, i32 0)) #5
  %10 = load i8** %str_to_ret, align 8
  %11 = getelementptr inbounds i8* %10, i64 %9
  store i8 0, i8* %11, align 1
  %12 = load i8** %str_to_ret, align 8
  ret i8* %12
}

declare i32 @__isoc99_scanf(i8*, ...) #1
declare i8* @fgets(i8*, i32, %struct._IO_FILE*) #1

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
