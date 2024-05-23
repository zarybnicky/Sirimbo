import { Dialog, DialogContent, StdDialogTrigger } from '@/ui/dialog';
import { buttonCls } from '@/ui/style';
import * as React from 'react';

export function FormDialogButton<E extends ((...params: any[]) => React.ReactNode)>({
  intent = 'edit',
  cls = {},
  Form,
  ...props
}: {
  cls?: Parameters<typeof buttonCls>[0];
  intent?: 'add' | 'edit';
  Form: E;
} & (Parameters<E>[0] extends object ? Parameters<E>[0] : object)) {
  return (
    <Dialog>
      {intent === 'add' ? <StdDialogTrigger.Add {...cls} /> : <StdDialogTrigger.Edit {...cls} />}
      <DialogContent>
        {React.createElement(Form, props)}
      </DialogContent>
    </Dialog>
  );
}
