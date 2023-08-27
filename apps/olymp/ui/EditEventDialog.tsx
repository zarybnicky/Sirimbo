import { EventDocument } from '@app/graphql/Event';
import { Dialog, DialogContent, DialogTrigger } from '@app/ui/dialog';
import { buttonCls } from '@app/ui/style';
import { useZodForm } from 'lib/use-schema-form';
import { Edit } from 'lucide-react';
import React from 'react';
import { useQuery } from 'urql';
import { TypeOf, z } from 'zod';
import { ErrorPage } from './ErrorPage';
import { useAsyncCallback } from 'react-async-hook';
import { SubmitButton } from './submit';

const Form = z.object({
});

export const EditEventDialog = ({ id = '' }: { id?: string }) => {
  const [open, setOpen] = React.useState(false);
  const [query] = useQuery({ query: EventDocument, variables: { id }, pause: !id });
  const data = query.data?.event;

  const { reset, control, handleSubmit } = useZodForm(Form);
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [reset, data]);

  const onSubmit = useAsyncCallback((values: TypeOf<typeof Form>) => {

  });

  if (query.data && query.data.event === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <button className={buttonCls({ variant: 'outline' })}>
          <Edit />
          Upravit
        </button>
      </DialogTrigger>

      <DialogContent>
        <form className="grid lg:grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit.execute)}>

          <div className="col-span-2">
             <SubmitButton loading={onSubmit.loading} />
          </div>
        </form>
      </DialogContent>
    </Dialog>
  );
};
