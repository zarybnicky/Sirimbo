import React from 'react';
import classNames from 'classnames';
import { CircularProgress } from '@mui/material';
import KeyboardArrowRightIcon from '@mui/icons-material/KeyboardArrowRight';

export const SubmitButton = React.forwardRef(function SubmitButton({
  loading, disabled, fill, danger, children, ...props
}: Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'className' | 'type' | 'disabled'> & {
  fill?: boolean;
  disabled?: boolean;
  loading?: boolean;
  danger?: boolean;
}, ref: React.ForwardedRef<HTMLButtonElement>) {
  return <button type="submit" {...props} ref={ref} disabled={loading || disabled} className={classNames(
    fill ? 'w-full flex justify-center' : '',
    danger
      ? (loading ? 'bg-red-800' : 'bg-red-500 hover:bg-red-700')
      : (loading ? 'bg-tertiary' : 'bg-primary hover:bg-secondary'),
    danger ? 'border-red-700' : 'border-tertiary',
    'button-medium py-2 px-4 font-medium text-white border',
  )}>
    {children}
    {loading ? <CircularProgress /> : <KeyboardArrowRightIcon />}
  </button>;
});
