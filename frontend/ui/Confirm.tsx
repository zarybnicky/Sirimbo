import React from 'react';
import { Dialog, DialogContent, DialogTitle } from './dialog';
import { DialogClose } from '@radix-ui/react-dialog';
import { cn } from './cn';

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

      <Dialog open={!!resolve} onOpenChange={handleCancel}>
        <DialogContent>
          <DialogTitle>{options.title}</DialogTitle>
          {options.description}

          <div className="mt-4 flex justify-end">
            <DialogClose
              onClick={handleConfirm}
              className={cn(
                'inline-flex select-none justify-center rounded-md px-4 py-2 text-sm font-medium',
                'bg-red-600 text-white hover:bg-red-700',
                'border border-transparent',
                'focus:outline-none focus-visible:ring focus-visible:ring-red-500/75',
              )}
            >
              {options.confirmationText}
            </DialogClose>
          </div>

        </DialogContent>
      </Dialog>
    </React.Fragment>
  );
});
