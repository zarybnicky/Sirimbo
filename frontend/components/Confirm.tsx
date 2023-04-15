import React from 'react';
import { Transition } from '@headlessui/react';
import * as DialogPrimitive from '@radix-ui/react-dialog';
import classNames from 'classnames';
import { X as CloseIcon } from 'react-feather';

type ConfirmOptions = {
  title: React.ReactNode;
  description: React.ReactNode;
  content: React.ReactNode;
  confirmationText: string;
  cancellationText: string;
  allowClose: boolean;
};

const DEFAULT_OPTIONS: ConfirmOptions = {
  title: 'Jste si jistí?',
  description: '',
  content: null,
  confirmationText: 'Ok',
  cancellationText: 'Zrušit',
  allowClose: true,
};

const ConfirmContext = React.createContext<
  ((options: Partial<ConfirmOptions>) => Promise<unknown>) | null
>(null);

export const useConfirm = () => {
  const confirm = React.useContext(ConfirmContext);
  if (confirm === null) {
    throw new Error('You can only use `useConfirm` from inside a ConfirmProvider');
  }
  return confirm;
};

export const ConfirmProvider = React.memo(function ConfirmProvider({
  children,
}: {
  children: React.ReactNode;
}) {
  const [options, setOptions] = React.useState(DEFAULT_OPTIONS);
  const [resolveReject, setResolveReject] = React.useState<
    [] | [(value?: unknown) => void, () => void]
  >([]);
  const [resolve, reject] = resolveReject;

  const confirm = React.useCallback((options: Partial<ConfirmOptions> = {}) => {
    return new Promise((resolve, reject) => {
      setOptions({ ...DEFAULT_OPTIONS, ...options });
      setResolveReject([resolve, reject]);
    });
  }, []);

  const handleCancel = React.useCallback(
    (open: boolean) => {
      if (reject && !open) {
        reject();
        setResolveReject([]);
      }
    },
    [reject],
  );

  const handleConfirm = React.useCallback(() => {
    if (resolve) {
      resolve();
      setResolveReject([]);
    }
  }, [resolve]);

  return (
    <React.Fragment>
      <ConfirmContext.Provider value={confirm}>{children}</ConfirmContext.Provider>

      <DialogPrimitive.Root open={!!resolve} onOpenChange={handleCancel}>
        <Transition.Root show={!!resolve}>
          <Transition.Child
            as={React.Fragment}
            enter="ease-out duration-300"
            enterFrom="opacity-0"
            enterTo="opacity-100"
            leave="ease-in duration-200"
            leaveFrom="opacity-100"
            leaveTo="opacity-0"
          >
            <DialogPrimitive.Overlay
              forceMount
              className="fixed inset-0 z-20 bg-black/50"
            />
          </Transition.Child>
          <Transition.Child
            as={React.Fragment}
            enter="ease-out duration-300"
            enterFrom="opacity-0 scale-95"
            enterTo="opacity-100 scale-100"
            leave="ease-in duration-200"
            leaveFrom="opacity-100 scale-100"
            leaveTo="opacity-0 scale-95"
          >
            <DialogPrimitive.Content
              forceMount
              className={classNames(
                'fixed z-50',
                'w-[95vw] max-w-md rounded-lg p-4 md:w-full',
                'top-[50%] left-[50%] -translate-x-[50%] -translate-y-[50%]',
                'bg-white dark:bg-gray-800',
                'focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75',
              )}
            >
              <DialogPrimitive.Title className="text-sm font-medium text-gray-900 dark:text-gray-100">
                {options.title}
              </DialogPrimitive.Title>

              {options.description}

              <div className="mt-4 flex justify-end">
                <DialogPrimitive.Close
                  onClick={handleConfirm}
                  className={classNames(
                    'inline-flex select-none justify-center rounded-md px-4 py-2 text-sm font-medium',
                    'bg-purple-600 text-white hover:bg-purple-700 dark:bg-purple-700 dark:text-gray-100 dark:hover:bg-purple-600',
                    'border border-transparent',
                    'focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75',
                  )}
                >
                  {options.confirmationText}
                </DialogPrimitive.Close>
              </div>

              <DialogPrimitive.Close
                className={classNames(
                  'absolute top-3.5 right-3.5 inline-flex items-center justify-center rounded-full p-1',
                  'focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75',
                )}
              >
                <CloseIcon className="h-4 w-4 text-gray-500 hover:text-gray-700 dark:text-gray-500 dark:hover:text-gray-400" />
              </DialogPrimitive.Close>
            </DialogPrimitive.Content>
          </Transition.Child>
        </Transition.Root>
      </DialogPrimitive.Root>
    </React.Fragment>
  );
});
