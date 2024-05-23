import { PersonWithLinksFragment } from "@/graphql/Person";
import { AddToPersonButton } from "@/ui/AddToPersonButton";
import { EditUserProxyCard } from "@/ui/EditUserProxyCard";
import { FormDialogButton } from "@/ui/FormDialogButton";
import { fullDateFormatter } from "@/ui/format";
import { CreateInvitationForm } from "@/ui/forms/CreateInvitationForm";
import React from "react";

export function PersonAccessView({ item }: { item: PersonWithLinksFragment }) {
  return (
    <div className="prose prose-accent mb-2">
      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Přístupové údaje</h3>
        <AddToPersonButton person={item} />
      </div>

      {item.userProxiesList?.map(item => (
        <EditUserProxyCard key={item.id} data={item} />
      ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Pozvánky</h3>
        <FormDialogButton intent="add" Form={CreateInvitationForm} person={item} />
      </div>

      {item.personInvitationsList?.map(item => (
        <div key={item.id}>
          {item.email}, vytvořena {fullDateFormatter.format(new Date(item.createdAt))}
        </div>
      ))}
    </div>
  );
}
