import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { ComboboxElement } from '@app/ui/Combobox';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { PersonListDocument, PersonBasicFragment } from '@app/graphql/Person';
import { CreateCoupleDocument } from '@app/graphql/Memberships';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { Dialog, DialogContent, DialogTrigger } from './dialog';
import { buttonCls } from '@app/ui/style';
import { Plus } from 'lucide-react';

const Form = z.object({
  man: z.string(),
  woman: z.string(),
});
type FormProps = z.infer<typeof Form>;

export function CreateCoupleForm({ initial, onSuccess }: { initial?: PersonBasicFragment; onSuccess?: () => void }) {
  const [{ data }] = useQuery({ query: PersonListDocument });
  const men = React.useMemo(
    () =>
      (data?.filteredPeopleList || [])
        .filter((x) => x.gender === 'MAN')
        .map((x) => ({
          id: x.id,
          label: x.name + (x.birthDate ? ` (${new Date(x.birthDate).getFullYear()})` : ''),
        })),
    [data],
  );
  const women = React.useMemo(
    () =>
      (data?.filteredPeopleList || [])
        .filter((x) => x.gender === 'WOMAN')
        .map((x) => ({
          id: x.id,
          label: x.name + (x.birthDate ? ` (${new Date(x.birthDate).getFullYear()})` : ''),
        })),
    [data],
  );

  const { reset, control, handleSubmit } = useForm<FormProps>({ resolver: zodResolver(Form) });
  React.useEffect(() => {
    if (initial && initial.gender === 'MAN') {
      reset({ man: initial.id });
    } else if (initial && initial.gender === 'WOMAN') {
      reset({ woman: initial.id });
    }
  }, [initial]);

  const doCreate = useMutation(CreateCoupleDocument)[1];

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const res = await doCreate({
      input: {
        couple: {
          manId: values.man,
          womanId: values.woman,
          since: new Date().toISOString(),
        },
      },
    });
    const id = res.data?.createCouple?.couple?.id;
    if (id) {
      onSuccess?.();
    }
  });

  return (
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
  );
}

export function CreateCoupleButton() {
  const [open, setOpen] = React.useState(false);

  return (
    <Dialog open={open} onOpenChange={setOpen} modal={false}>
      <DialogTrigger asChild>
        <button className={buttonCls({ size: 'sm', variant: 'outline' })}>
          <Plus />
          Přidat pár
        </button>
      </DialogTrigger>
      <DialogContent>
        <CreateCoupleForm onSuccess={() => setOpen(false)} />
      </DialogContent>
    </Dialog>
  );
}
