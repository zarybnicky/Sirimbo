import { PersonWithLinksFragment } from "@/graphql/Person";
import React from "react";
import { AddToPersonButton } from "./AddToPersonForm";
import { EditUserProxyCard } from "./EditUserProxyForm";
import { fullDateFormatter } from "./format";
import { CreateInvitationButton } from "./CreateInvitationForm";

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
        <CreateInvitationButton person={item} />
      </div>

      {item.personInvitationsList?.map(item => (
        <div key={item.id}>
          {item.email}, vytvořena {fullDateFormatter.format(new Date(item.createdAt))}
        </div>
      ))}
    </div>
  );
}
