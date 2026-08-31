import React from 'react';
import { ChevronRight } from 'lucide-react';
import type { AsyncState } from 'react-async-hook';
import { type Control, type FieldValues, useFormState } from 'react-hook-form';
import { buttonCls } from '@/ui/style';
import { Spinner } from '@/ui/Spinner';

type BaseSubmitButtonProps = React.ComponentPropsWithRef<'button'> & {
  state?: AsyncState<unknown>['status'];
  variant?: NonNullable<Parameters<typeof buttonCls>[0]>['variant'];
};

type SubmitAction = AsyncState<unknown> & {
  execute: () => Promise<unknown>;
};

export type SubmitButtonProps<T extends FieldValues = FieldValues> =
  | (Omit<BaseSubmitButtonProps, 'state'> & {
      action?: SubmitAction;
      control?: never;
    })
  | (Omit<BaseSubmitButtonProps, 'state'> & {
      action?: never;
      control: Control<T>;
    });

export function SubmitButton<T extends FieldValues>({
  control,
  action,
  onClick,
  type,
  ...props
}: SubmitButtonProps<T>) {
  return control ? (
    <FormStateSubmitButton
      {...props}
      control={control}
      type={type}
      onClick={onClick}
    />
  ) : (
    <BaseSubmitButton
      {...props}
      state={action?.status}
      type={type ?? (action ? 'button' : undefined)}
      onClick={action?.execute ?? onClick}
    />
  );
}

function FormStateSubmitButton<T extends FieldValues>({
  control,
  state,
  disabled,
  type,
  ...props
}: BaseSubmitButtonProps & { control: Control<T> }) {
  const { isSubmitted, isSubmitting, isSubmitSuccessful } = useFormState({ control });
  const controlState = isSubmitting
    ? 'loading'
    : isSubmitSuccessful
      ? 'success'
      : isSubmitted
        ? 'error'
        : undefined;

  return (
    <BaseSubmitButton
      {...props}
      state={controlState ?? state}
      disabled={disabled}
    />
  );
}

function BaseSubmitButton({
  state: submitState = 'not-requested',
  disabled,
  className,
  children = 'Uložit',
  variant,
  type,
  ...props
}: BaseSubmitButtonProps) {
  const [state, setState] = React.useState<'NORMAL' | 'LOADING' | 'LOADED'>('NORMAL');
  React.useEffect(() => {
    if (submitState === 'loading') {
      setState('LOADING');
    } else if (submitState !== 'success') {
      setState('NORMAL');
    } else {
      setState('LOADED');
      const timeout = setTimeout(() => setState('NORMAL'), 1000);
      return () => clearTimeout(timeout);
    }
  }, [submitState]);

  return (
    <button
      type={type ?? 'submit'}
      {...props}
      disabled={submitState === 'loading' || disabled}
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
        className="animate-dash [stroke-dasharray:1000] [stroke-dashoffset:0]"
        fill="none"
        stroke="currentColor"
        strokeWidth="6"
        strokeMiterlimit="10"
        cx="65.1"
        cy="65.1"
        r="62.1"
      />
      <polyline
        className="animate-dash-check [stroke-dasharray:1000] [stroke-dashoffset:-100]"
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
