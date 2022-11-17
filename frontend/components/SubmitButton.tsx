import React from 'react';
import { ChevronRight } from 'react-feather';
import classNames from 'classnames';
import { Spinner } from './Spinner';

export const SubmitButton = React.forwardRef(function SubmitButton({
  loading, disabled, className, children = "Uložit",
  ...props
}: Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'type' | 'disabled'> & {
  disabled?: boolean;
  loading?: boolean;
}, ref: React.ForwardedRef<HTMLButtonElement>) {
  const color = disabled ? 'button-secondary' : 'button-red';
  return <button
    type="submit" {...props} ref={ref}
    disabled={loading || disabled}
    className={classNames("button text-sm justify-center items-center col-full flex", color, className)}
  >
    {children}
    {loading ? <Spinner /> : <ChevronRight />}
  </button>;
});
