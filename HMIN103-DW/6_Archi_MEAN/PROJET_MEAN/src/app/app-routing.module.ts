import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
// import { ConnexionComponent } from './connexion/connexion.component';
import { CategoriesComponent } from './categories/categories.component';
import { CategoriesComponent } from './categories/categories.component';
import { ProduitsComponent } from './produits/panier.component';

const routes: Routes = [
  { path: 'membres/connexion',
    component: ConnexionComponent
  },
  { path: 'categories',
    component: CategoriesComponent
  },
  { path: 'produits/:categorie',
    component: ProduitsComponent
  },
  { path: 'produits',
    component: ProduitsComponent
  }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]  
})
export class AppRoutingModule { } 
