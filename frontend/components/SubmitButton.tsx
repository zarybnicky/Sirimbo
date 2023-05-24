import React from 'react';
import { ChevronRight } from 'react-feather';
import classNames from 'classnames';
import { Spinner } from './Spinner';

export const SubmitButton = React.forwardRef(function SubmitButton(
  {
    loading,
    disabled,
    className,
    children = 'Uložit',
    ...props
  }: Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'disabled'> & {
    disabled?: boolean;
    loading?: boolean;
  },
  ref: React.ForwardedRef<HTMLButtonElement>,
) {
  const [state, setState] = React.useState<'NORMAL' | 'LOADING' | 'LOADED'>('NORMAL');
  React.useEffect(() => {
    setState((oldState) => {
      if (loading) return 'LOADING';
      if (oldState === 'LOADING') {
        setTimeout(() => setState('NORMAL'), 1000);
        return 'LOADED';
      } else {
        return 'NORMAL';
      }
    });
  }, [loading]);

  const color = state !== 'NORMAL' ? 'button-white' : disabled ? 'button-secondary' : 'button-red';
  return (
    <button
      type="submit"
      {...props}
      ref={ref}
      disabled={loading || disabled}
      className={classNames(
        'button text-sm justify-center items-center col-full flex',
        color,
        className,
      )}
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
});

const AnimatedCheck = () => (
  <svg version="1.1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 130.2 130.2">
    <circle
      className="animated circle"
      fill="none"
      stroke="currentColor"
      stroke-width="6"
      stroke-miterlimit="10"
      cx="65.1"
      cy="65.1"
      r="62.1"
    />
    <polyline
      className="animated check"
      fill="none"
      stroke="currentColor"
      stroke-width="6"
      stroke-linecap="round"
      stroke-miterlimit="10"
      points="100.2,40.2 51.5,88.8 29.8,67.5 "
    />
  </svg>
);
