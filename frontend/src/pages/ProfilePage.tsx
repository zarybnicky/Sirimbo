import * as React from 'react';
import format from 'date-fns/format';
import { Container, Grid, Typography } from '@material-ui/core';
import { useTypedQuery } from '../zeus/apollo';
import { useAuth } from '../data/use-auth';

export const ProfilePage = ({ }) => {
  const { user } = useAuth();

  /*
    <div class="mb-2">
      <a class="btn btn-outline-primary" href="/member/profil/edit">Upravit osobní údaje</a>
      <a class="btn btn-outline-primary" href="/member/profil/heslo">Změnit heslo</a>
      <a class="btn btn-outline-primary" href="/member/profil/par/partner">Změnit partnera</a>
      <a class="btn btn-outline-primary" href="/logout">Odhlásit se</a>
    </div>

  <div class="mb-2">
    {% if coupleData.u_id %}
      {% if user.gender == 'm' %}Partnerka:{% else %}Partner:{% endif %}
      {{ coupleData.u_jmeno }} {{ coupleData.u_prijmeni }}<br>
    {% endif %}
    Tréninková skupina:
    <div class="box" title="{{ skupina.name }}" style="background-color:{{ skupina.color }}"></div>
    &nbsp;{{ skupina.name }}<br>
    Variabilní symbol: {{ user.varSymbol }}<br>
    Věková kategorie: {{ ageGroup }}<br>
    Členské příspěvky: {{ hasPaid ? 'zaplaceno' : 'nezaplaceno' }}<br>
  </div>

  {% if paymentsWanted %}
    <h3 class="mt-2">K zaplacení</h3>
    {% for item in paymentsWanted %}
      <div class="modal fade" id="qrModal-{{ item.id }}" tabindex="-1"
           role="dialog" aria-hidden="true">
        <div class="modal-dialog modal-dialog-centered">
          <div class="modal-content">
            <div class="modal-header">
              <h5 class="modal-title">QR platba</h5>
              <button type="button" class="close" data-dismiss="modal" aria-label="Close">
                <span aria-hidden="true">&times;</span>
              </button>
            </div>
            <div class="modal-body d-flex align-items-center justify-content-center">
              <qr-payment
                  acc="1806875329/0800"
                  am="{{ item.amount }}"
                  msg="{{ user.fullName ~ ': ' ~ item.name }}"
                  ss="{{ item.symbol }}"
                  vs="{{ user.varSymbol }}"
              ></qr-payment>
            </div>
          </div>
        </div>
      </div>
      <div class="card mb-1" style="max-width: 600px">
        <div class="card-body p-2">
          <div class="row no-gutters justify-content-between">
            <div class="col-12 col-md-auto h5 mb-0">{{ item.name }}</div>
            <div class="col-auto">do {{ item.dueDate|date('j. n. Y') }}</div>
          </div>
          <div class="row no-gutters">
            <div class="col-12 col-md-6">{{ item.amount }} Kč</div>
            <div class="col-6 col-md-3">SS: {{ item.symbol }}</div>
            <div class="col-6 col-md-3 text-right">VS: {{ user.varSymbol }}</div>
          </div>
          <div class="row no-gutters justify-content-between">
            <div class="col-12 order-2 order-md-1 col-md-auto">
              <button type="button" class="btn btn-link p-0" data-toggle="modal"
                      data-target="#qrModal-{{ item.id }}">
                <img width="16" alt="QR platba" src="/style/icon-qr-code.png" style="vertical-align:text-top" />
                QR platba
              </button>
            </div>
            <div class="col-12 order-1 order-md-2 col-md-auto">č.ú. 1806875329/0800</div>
          </div>
        </div>
      </div>
    {% endfor %}
  {% endif %}

  {% if paymentHistory %}
    <h3 class="mt-2">Historie plateb</h3>
    {% for item in paymentHistory %}
      <div class="card mb-1" style="max-width: 600px">
        <div class="card-body p-2">
          <div class="row no-gutters justify-content-between">
            <div class="col-12 col-md-auto h5 mb-0">{{ item.name }}</div>
            <div class="col-auto">placeno {{ item.paidOn|date('j. n. Y') }}</div>
          </div>
          <div class="row no-gutters">
            <div class="col-12 col-md-6">{{ item.amount }} Kč</div>
            <div class="col-6 col-md-3">SS: {{ item.symbol }}</div>
            <div class="col-6 col-md-3 text-right">VS: {{ user.varSymbol }}</div>
          </div>
          <div class="row no-gutters justify-content-between">
            <div class="col-12 order-2 order-md-1 col-md-auto">
              <date-range from="{{ item.validFrom }}" to="{{ item.validUntil }}"></date-range>
            </div>
            <div class="col-12 order-1 order-md-2 col-md-auto">č.ú. 1806875329/0800</div>
          </div>
        </div>
      </div>
    {% endfor %}
  {% endif %}

  */
  return <Container maxWidth="lg" style={{ padding: '2rem 0' }}>
    <Typography variant="h4" component="h2">Profil</Typography>
    <Grid container spacing={2}>
      <Grid item>
        Profile
      </Grid>
    </Grid>
  </Container>;
}