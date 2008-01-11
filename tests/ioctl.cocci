@fn@
identifier xyz_ioctl;
identifier xyz_ops;
@@

struct file_operations xyz_ops = {
	.ioctl = xyz_ioctl,
};

@safe@
identifier fn.xyz_ioctl;
identifier i, f, cmd, arg;
@@

static int xyz_ioctl(struct inode *i, ...)
{
  ... when != i
}

@one depends on safe@
identifier fn.xyz_ioctl;
identifier i, f, cmd, arg;
identifier ret;
constant cret;
type T;
identifier id;
statement S,S1;
@@

int xyz_ioctl(
-             struct inode *i,
              ...)
{
  ... when != S1
+ lock_kernel();
  S
  ...
(
+ unlock_kernel();
  return ret;
|
+ unlock_kernel();
  return cret;
)
}

@call depends on one@
identifier fn.xyz_ioctl;
expression E;
@@

xyz_ioctl(
-         E,
          ...)


// be sure the changes can be made before transforming
// prototype has to be more complicated, because unsigned int can be
// just unsigned
@decl depends on one@
identifier xyz_ioctl;
identifier xyz_ops;
@@

struct file_operations xyz_ops = {
-	.ioctl = xyz_ioctl,
+	.unlocked_ioctl = xyz_ioctl,
};
