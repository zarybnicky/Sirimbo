type Variants = 'h1' | 'h2' | 'h3';

type Props = {
  variant: Variants;
  children: React.ReactNode;
};

const classes: { [v in Variants]: string } = {
  h1: '',
  h2: '',
  h3: '',
};

export function Typography({ variant, children }: Props) {
  return <div className={classes[variant]}>{children}</div>;
}
