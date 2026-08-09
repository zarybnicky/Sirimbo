import React from 'react';
import { ChevronRight } from 'lucide-react';
import { type Control, type FieldValues, useFormState } from 'react-hook-form';
import { buttonCls } from '@/ui/style';
import { Spinner } from '@/ui/Spinner';

type BaseSubmitButtonProps = React.ComponentPropsWithRef<'button'> & {
  loading?: boolean;
  variant?: NonNullable<Parameters<typeof buttonCls>[0]>['variant'];
};

export type SubmitButtonProps<T extends FieldValues = FieldValues> =
  BaseSubmitButtonProps & { control?: Control<T> };

export function SubmitButton<T extends FieldValues>({
  control,
  ...props
}: SubmitButtonProps<T>) {
  return control ? (
    <FormStateSubmitButton {...props} control={control} />
  ) : (
    <BaseSubmitButton {...props} />
  );
}

function FormStateSubmitButton<T extends FieldValues>({
  control,
  loading,
  disabled,
  ...props
}: BaseSubmitButtonProps & { control: Control<T> }) {
  const { isSubmitting } = useFormState({ control });

  return (
    <BaseSubmitButton
      {...props}
      loading={loading || isSubmitting}
      disabled={disabled}
    />
  );
}

function BaseSubmitButton({
  loading,
  disabled,
  className,
  children = 'Uložit',
  variant,
  ...props
}: BaseSubmitButtonProps) {
  const [state, setState] = React.useState<'NORMAL' | 'LOADING' | 'LOADED'>('NORMAL');
  React.useEffect(() => {
    setState((oldState) => {
      if (loading) {
        return 'LOADING';
      }
      if (oldState === 'LOADING') {
        setTimeout(() => setState('NORMAL'), 1000);
        return 'LOADED';
      }
      return 'NORMAL';
    });
  }, [loading]);

  return (
    <button
      type="submit"
      {...props}
      disabled={loading || disabled}
      className={buttonCls({
        className,
        variant: variant || (state === 'NORMAL' ? 'primary' : 'outline'),
      })}
    >
      {state === 'NORMAL' ? (
        <>
          {children}
          <ChevronRight />
        </>
      ) : state === 'LOADING' ? (
        <Spinner />
      ) : state === 'LOADED' ? (
        <AnimatedCheck />
      ) : (
        children
      )}
    </button>
  );
}

function AnimatedCheck() {
  return (
    <svg
      role="presentation"
      version="1.1"
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 130.2 130.2"
    >
      <circle
        className="animated circle"
        fill="none"
        stroke="currentColor"
        strokeWidth="6"
        strokeMiterlimit="10"
        cx="65.1"
        cy="65.1"
        r="62.1"
      />
      <polyline
        className="animated check"
        fill="none"
        stroke="currentColor"
        strokeWidth="6"
        strokeLinecap="round"
        strokeMiterlimit="10"
        points="100.2,40.2 51.5,88.8 29.8,67.5 "
      />
    </svg>
  );
}
