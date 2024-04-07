import React from 'react';
import { ChevronRight } from 'lucide-react';
import { buttonCls } from '@/ui/style';

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

  return (
    <button
      type="submit"
      {...props}
      ref={ref}
      disabled={loading || disabled}
      className={buttonCls({ className, variant: state === 'NORMAL' ? 'primary' : 'outline' })}
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

const Spinner = React.memo(function Spinner() {
  return (
    <svg
      className="size-6 animate-spin"
      viewBox="0 0 24 24"
      fill="none"
      xmlns="http://www.w3.org/2000/svg"
    >
      <path
        d="M12 4.75V6.25"
        stroke="currentColor"
        strokeWidth="1.5"
        strokeLinecap="round"
        strokeLinejoin="round"
      ></path>
      <path
        d="M17.1266 6.87347L16.0659 7.93413"
        stroke="currentColor"
        strokeWidth="1.5"
        strokeLinecap="round"
        strokeLinejoin="round"
      ></path>
      <path
        d="M19.25 12L17.75 12"
        stroke="currentColor"
        strokeWidth="1.5"
        strokeLinecap="round"
        strokeLinejoin="round"
      ></path>
      <path
        d="M17.1266 17.1265L16.0659 16.0659"
        stroke="currentColor"
        strokeWidth="1.5"
        strokeLinecap="round"
        strokeLinejoin="round"
      ></path>
      <path
        d="M12 17.75V19.25"
        stroke="currentColor"
        strokeWidth="1.5"
        strokeLinecap="round"
        strokeLinejoin="round"
      ></path>
      <path
        d="M7.9342 16.0659L6.87354 17.1265"
        stroke="currentColor"
        strokeWidth="1.5"
        strokeLinecap="round"
        strokeLinejoin="round"
      ></path>
      <path
        d="M6.25 12L4.75 12"
        stroke="currentColor"
        strokeWidth="1.5"
        strokeLinecap="round"
        strokeLinejoin="round"
      ></path>
      <path
        d="M7.9342 7.93413L6.87354 6.87347"
        stroke="currentColor"
        strokeWidth="1.5"
        strokeLinecap="round"
        strokeLinejoin="round"
      ></path>
    </svg>
  );
});
