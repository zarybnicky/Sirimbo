import * as DialogPrimitive from '@radix-ui/react-dialog';
import cx from 'classnames';
import React from 'react';
import { X as CloseIcon } from 'react-feather';

type Props = {
  title?: React.ReactNode;
  children: React.ReactNode | ((props: { close: () => void }) => React.ReactNode);
  button: React.ReactNode;
};

export const SimpleDialog = ({ title, children, button }: Props) => {
  const [isOpen, setIsOpen] = React.useState(false);
  const contentProps = React.useMemo(() => ({ close: () => setIsOpen(false) }), []);

  return (
    <div>
      <DialogPrimitive.Root open={isOpen} onOpenChange={setIsOpen}>
        <DialogPrimitive.Trigger asChild>{button}</DialogPrimitive.Trigger>

        <DialogPrimitive.Portal>
          <DialogPrimitive.Overlay
            className={cx(
              'bg-black/50 fixed inset-0 z-20',
              'data-[state=open]:animate-overlayShow data-[state=closed]:animate-overlayHide',
            )}
          />
          <DialogPrimitive.Content
            forceMount
            className={cx(
              'fixed z-50 overflow-y-auto',
              'w-[95vw] max-h-[95vh] max-w-xl rounded-lg p-4 md:w-full',
              'top-[50%] left-[50%] -translate-x-[50%] -translate-y-[50%]',
              'bg-white dark:bg-gray-800',
              'focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75',
              'data-[state=open]:animate-contentShow data-[state=closed]:animate-contentHide',
            )}
          >
            {title && (
              <DialogPrimitive.Title className="text-sm font-medium text-gray-900 dark:text-gray-100">
                {title}
              </DialogPrimitive.Title>
            )}

            {typeof children === 'function' ? children(contentProps) : children}

            <DialogPrimitive.Close
              className={cx(
                'absolute top-3.5 right-3.5 inline-flex items-center justify-center rounded-full p-1',
                'focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75',
              )}
            >
              <CloseIcon className="h-4 w-4 text-gray-500 hover:text-gray-700 dark:text-gray-500 dark:hover:text-gray-400" />
            </DialogPrimitive.Close>
          </DialogPrimitive.Content>
        </DialogPrimitive.Portal>
      </DialogPrimitive.Root>
    </div>
  );
};
