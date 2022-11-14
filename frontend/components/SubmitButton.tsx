import React from 'react';
import KeyboardArrowRightIcon from '@mui/icons-material/KeyboardArrowRight';
import classNames from 'classnames';

const Spinner = React.memo(function Spinner() {
  return (
    <svg className="w-10 h-10 animate-spin text-indigo-400" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path d="M12 4.75V6.25" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"></path>
      <path d="M17.1266 6.87347L16.0659 7.93413" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"></path>
      <path d="M19.25 12L17.75 12" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"></path>
      <path d="M17.1266 17.1265L16.0659 16.0659" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"></path>
      <path d="M12 17.75V19.25" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"></path>
      <path d="M7.9342 16.0659L6.87354 17.1265" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"></path>
      <path d="M6.25 12L4.75 12" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"></path>
      <path d="M7.9342 7.93413L6.87354 6.87347" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"></path>
    </svg>
  );
});

export const SubmitButton = React.forwardRef(function SubmitButton({
  loading, disabled, className, children = "Uložit",
  ...props
}: Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'type' | 'disabled'> & {
  disabled?: boolean;
  loading?: boolean;
}, ref: React.ForwardedRef<HTMLButtonElement>) {
  const color = disabled ? 'button button-secondary' : 'button button-red';
  return <button
    type="submit" {...props} ref={ref}
    disabled={loading || disabled}
    className={classNames(color, className)}
  >
    {children}
    {loading ? <Spinner /> : <KeyboardArrowRightIcon />}
  </button>;
});
