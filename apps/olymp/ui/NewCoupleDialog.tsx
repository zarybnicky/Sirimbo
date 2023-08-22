import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { ComboboxElement } from '@app/ui/Combobox';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { PersonListDocument } from '@app/graphql/Person';
import { CreateCoupleDocument } from '@app/graphql/Couple';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { useAuth } from './use-auth';
import { Dialog, DialogContent, DialogTrigger } from './dialog';
import { buttonCls } from '@app/ui/style';
import { Plus } from 'lucide-react';

const Form = z.object({
  man: z.string(),
  woman: z.string(),
});
type FormProps = z.infer<typeof Form>;

export function NewCoupleDialog({ onSuccess }: { onSuccess?: () => void }) {
  const [open, setOpen] = React.useState(false);
  const { tenants } = useAuth();
  const [{ data }] = useQuery({
    query: PersonListDocument,
    variables: { inTenants: tenants.map((x) => x.id) },
  });
  const men = React.useMemo(
    () =>
      (data?.filteredPeopleList || [])
        .filter((x) => x.gender === 'MAN')
        .map((x) => ({
          id: x.id,
          label: `${x.firstName} ${x.lastName} (${new Date(x.birthDate).getFullYear()})`,
        })),
    [data],
  );
  const women = React.useMemo(
    () =>
      (data?.filteredPeopleList || [])
        .filter((x) => x.gender === 'WOMAN')
        .map((x) => ({
          id: x.id,
          label: `${x.firstName} ${x.lastName} (${new Date(x.birthDate).getFullYear()})`,
        })),
    [data],
  );

  const doCreate = useMutation(CreateCoupleDocument)[1];

  const { control, handleSubmit } = useForm<FormProps>({ resolver: zodResolver(Form) });
  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doCreate({
      input: {
        couple: {
          manId: values.man,
          womanId: values.woman,
          active: true,
          since: new Date().toISOString(),
        },
      },
    });
    onSuccess?.();
  });

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger>
        <button className={buttonCls({ size: 'sm', variant: 'outline' })}>
          <Plus />
          Přidat pár
        </button>
      </DialogTrigger>
      <DialogContent>
        <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
          <FormError error={onSubmit.error} />
          <ComboboxElement
            control={control}
            name="man"
            label="Partner"
            placeholder="vyberte partnera"
            options={men}
          />
          <ComboboxElement
            control={control}
            name="woman"
            label="Partnerka"
            placeholder="vyberte partnerku"
            options={women}
          />
          <SubmitButton loading={onSubmit.loading}>Spárovat</SubmitButton>
        </form>
      </DialogContent>
    </Dialog>
  );
}
