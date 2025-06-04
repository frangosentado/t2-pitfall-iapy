"""
interface.py

define uma camada de comunicação entre Python e Prolog (pyswip),
carregando o monolítico pitfall.pro e permitindo assert/retract/query.
"""

from pyswip import Prolog

class PrologInterface:
    def __init__(self, prolog_file: str):
        """
        inicializa o interpretador Prolog e carrega o arquivo único pitfall.pro.
        prolog_file: caminho para pitfall.pro
        """
        self.prolog = Prolog()
        # carrega apenas pitfall.pro, que já contém fatos iniciais, regras e decisão
        self.prolog.consult(prolog_file)

    def assert_fact(self, template: str):
        """
        recebe uma string representando o corpo de um fato Prolog,
        ex: "poco(3,4)".
        Executa assertz(poco(3,4)). para inserir esse fato dinamicamente.
        """
        cmd = f"assertz({template})."
        list(self.prolog.query(cmd))  # força execução

    def retract_all(self, template: str):
        """
        executa retractall(template). Por ex: template="estado(_,_,_,_,_)".
        remove todos os fatos que unificam com esse padrão.
        """
        cmd = f"retractall({template})."
        list(self.prolog.query(cmd))

    def query(self, goal: str):
        """
        executa uma consulta Prolog pura (sem ponto final). Ex: "poco(X,Y)".
        Retorna um gerador de dicionários {variável: valor} com as soluções.
        """
        return self.prolog.query(goal)

    def get_next_action(self):
        """
        supõe que exista next_action(At) em pitfall.pro.
        Consulta e retorna a string unificada em At. Se não houver solução, retorna None.
        """
        respostas = list(self.prolog.query("next_action(At)"))
        if not respostas:
            return None
        return respostas[0]["At"]

    def move_agent(self, action: str):
        """
        supõe que exista em pitfall.pro um predicado move/1 que chama executa_acao/1.
        Por exemplo: move(andar). Ajuste caso o seu pitfall.pro use outro nome.
        """
        cmd = f"move({action})."
        list(self.prolog.query(cmd))

    def close(self):
        """
        finaliza o interpretador Prolog (libera recursos).
        """
        self.prolog = None